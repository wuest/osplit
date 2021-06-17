module ViewOnly exposing ( main )

import Browser
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Encode as JE
import Json.Decode as JD
import Dict as Dict

import Util exposing (..)
import Websocket as WS
import Timer as Timer
import Time as T

type alias CSKey       = String
type alias CSVal       = String
type alias ConfigStore = Dict.Dict CSKey CSVal

type alias Displayable = { hours   : Int
                         , minutes : Int
                         , seconds : Int
                         , millis  : Int
                         , sign    : String
                         }

type Msg = OpenSocket String
         | SendSocket JE.Value
         | SocketOpened WS.Socket
         | SocketNotOpened JD.Error
         | SocketReceived JE.Value
         -- UI
         | LoadSplits (Maybe Int)
         | CloseSplits
         -- Timer Controls
         | StartSplit (Maybe T.Posix)
         | Unsplit
         | Skip
         | Stop
         | Reset Timer.Timer
         -- Timer backend
         | Tick T.Posix

type WebsocketMessage = ClientStateRequest Int
                      | SplitsControl SplitsMessage
                      | UnloadSplits
                      | FetchedGameList (List Timer.Game)
                      | FetchedCategoryList (List Timer.Category)
                      | FetchedSplits Timer.SplitsSpec
                      | ConfigStoreSet CSKey CSVal

type SplitsMessage = RemoteStartSplit Int
                   | RemoteUnsplit
                   | RemoteSkip
                   | RemoteStop
                   | RemoteReset

type alias Model = { socket           : Maybe WS.Socket
                   , timer            : Timer.Timer
                   , gameList         : List Timer.Game
                   , categoryList     : List Timer.Category
                   , configStore      : ConfigStore
                   , maxSegmentsShown : Int
                   }

main = Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

init : String -> (Model, Cmd Msg)
init url = ( { socket = Nothing
             , timer = Timer.empty
             , gameList = []
             , categoryList = []
             , configStore = Dict.empty
             , maxSegmentsShown = 10
             }
           , WS.open url
           )

send : Maybe WS.Socket -> JE.Value -> Cmd msg
send ms v = case ms of
    Nothing -> Cmd.none
    Just s -> WS.send s v

{- UPDATE -}
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        OpenSocket url ->
            (model, WS.open url)
        SendSocket value ->
            (model, send (.socket model) value)
        SocketOpened newsocket ->
            ({ model | socket = Just newsocket }, WS.send newsocket newClientJSON)
        SocketNotOpened _ ->
            (model, Cmd.none)
        SocketReceived data ->
            processData model data
        {- Timer functions -}
        Tick t ->
            ({ model | timer = Timer.setTime (.timer model) t }, Cmd.none)
        StartSplit _ ->
            timerControl model msg
        Reset _ ->
            timerControl model msg
        Unsplit ->
            timerControl model msg
        Skip ->
            timerControl model msg
        CloseSplits ->
            ({ model | timer = Timer.empty }, Cmd.none)
        LoadSplits _ ->
            (model, Cmd.none)
        {- Blackhole everything else -}
        _ -> (model, Cmd.none)

timerControl : Model -> Msg -> (Model, Cmd Msg)
timerControl model msg =
    case msg of
        StartSplit t ->
            let offsetTime = Maybe.map (\time -> T.millisToPosix <| (T.posixToMillis time) - (Maybe.withDefault 0 (Maybe.map T.posixToMillis <| .runStarted <| Timer.splitsFor <| .timer model))) t
            in case .timer model of
                   Timer.Running _  -> ({ model | timer = Timer.split t <| .timer model }, Cmd.none)
                   Timer.Finished _ -> ({ model | timer = Timer.reset <| .timer model }, Cmd.none)
                   _                -> ({ model | timer = Timer.start t <| .timer model }, Cmd.none)
        Reset _ ->
            ({ model | timer = Timer.reset <| .timer model }, Cmd.none)
        Unsplit ->
            ({ model | timer = Timer.unsplit <| .timer model }, Cmd.none)
        Skip ->
            ({ model | timer = Timer.skip <| .timer model }, Cmd.none)
        _ -> (model, Cmd.none)

-- OUTBOUND COMMUNICATION

newClientJSON : JE.Value
newClientJSON = JE.object [ ( "tag", JE.string "NewClient" )
                         , ( "contents", JE.null )
                         ]

splitsLoadRequestJSON : Int -> JE.Value
splitsLoadRequestJSON category = JE.object [ ( "tag", JE.string "Menu" )
                                           , ( "contents", JE.object [ ( "tag", JE.string "MenuLoadSplits")
                                                                     , ( "contents", JE.int category )
                                                                     ]
                                             )
                                           ]

-- INCOMING MESSAGE PROCESSING

checkTag : String -> JD.Decoder a -> JD.Decoder a
checkTag target da = checkTag_ (JD.at [ "data", "tag" ] JD.string) target da

checkTag2 : String -> JD.Decoder a -> JD.Decoder a
checkTag2 target da = checkTag_ (JD.at [ "data", "contents", "tag" ] JD.string) target da

checkTag_ : JD.Decoder String -> String -> JD.Decoder a -> JD.Decoder a
checkTag_ ds target da =
    ds |> JD.andThen (\s -> if s == target then da else JD.fail ("No match on tag:" ++ s))

processData : Model -> JE.Value -> (Model, Cmd Msg)
processData model data =
    case processIncomingEvent data of
--        Ok (TimeSync response)        -> ( processTimeSync model response, Cmd.none )
        Ok (SplitsControl ctl)        -> processSplitsControl model ctl
        Ok (UnloadSplits)             -> ( { model | timer = Timer.empty }, Cmd.none )
        Ok (FetchedGameList games)    -> ( { model | gameList = games }, Cmd.none )
        Ok (FetchedCategoryList cats) -> ( { model | categoryList = cats }, Cmd.none )
        Ok (FetchedSplits s)          -> ( { model | timer = Timer.load s }, Cmd.none )
        Ok (ConfigStoreSet k v)       -> ( { model | configStore = Dict.insert k v (.configStore model) }, send (.socket model) (splitsLoadRequestJSON <| Maybe.withDefault -1 <| String.toInt v))
        _                             -> ( model, Cmd.none )

processSplitsControl : Model -> SplitsMessage -> (Model, Cmd Msg)
processSplitsControl model msg = case msg of
    RemoteStartSplit t -> timerControl model (StartSplit <| Maybe.map (\time -> T.millisToPosix <| (T.posixToMillis time) + t) (.runStarted <| Timer.splitsFor <| .timer model))
    RemoteUnsplit      -> timerControl model Unsplit
    RemoteSkip         -> timerControl model Skip
    RemoteStop         -> timerControl model Stop
    RemoteReset        -> timerControl model (Reset <| .timer model)

processIncomingEvent : JE.Value -> Result JD.Error WebsocketMessage
processIncomingEvent =
    JD.decodeValue <| JD.oneOf [ clientStateRequestDecoder
                               , splitsControlDecoder
                               , gameListDecoder
                               , categoryListDecoder
                               , splitsSpecDecoder
                               , closeSplitsDecoder
                               , splitsControlDecoder
                               , configStoreDecoder
                               ]

closeSplitsDecoder : JD.Decoder WebsocketMessage
closeSplitsDecoder = checkTag "CloseSplits" <| JD.map (\_ -> UnloadSplits) ( JD.at [ "data", "tag" ] JD.string )

gameListDecoder : JD.Decoder WebsocketMessage
gameListDecoder =
    checkTag "GameList" <| JD.map FetchedGameList
        ( JD.at [ "data", "contents" ] <| JD.list
            ( JD.map4 Timer.Game ( JD.at [ "gameID" ] <| JD.maybe JD.int )
                                 ( JD.at [ "gameData", "gameName" ] JD.string )
                                 ( JD.at [ "gameData", "gameIcon" ] <| JD.maybe JD.string )
                                 ( JD.at [ "gameData", "gameDefaultOffset" ] JD.int )
            )
        )

categoryListDecoder : JD.Decoder WebsocketMessage
categoryListDecoder =
    checkTag "CategoryList" <| JD.map FetchedCategoryList
        ( JD.at [ "data", "contents" ] <| JD.list
            ( JD.map3 Timer.Category ( JD.at [ "categoryID" ] <| JD.maybe JD.int )
                                     ( JD.at [ "categoryData", "categoryName" ] JD.string )
                                     ( JD.at [ "categoryData", "categoryOffset" ] JD.int )
            )
        )

configStoreDecoder : JD.Decoder WebsocketMessage
configStoreDecoder =
    checkTag "ConfigStore" <| JD.map2 ConfigStoreSet ( JD.at [ "data", "contents" ] <| JD.index 0 JD.string )
                                                     ( JD.at [ "data", "contents" ] <| JD.index 1 JD.string )

splitsSpecDecoder : JD.Decoder WebsocketMessage
splitsSpecDecoder =
    checkTag "SplitsRefresh" <| JD.map FetchedSplits <|
        JD.map3 Timer.SplitsSpec ( JD.at [ "data", "contents", "splitSetGameData", "gameName" ] JD.string )
                                 ( JD.at [ "data", "contents", "splitSetCategoryData", "categoryName" ] JD.string )
                                 ( JD.at [ "data", "contents" ] runSpecDecoder )

-- TODO: Implement multi-category
runSpecDecoder : JD.Decoder Timer.RunSpec
runSpecDecoder = JD.map Timer.SingleCategorySpec <|
    JD.map3 (Timer.Run Nothing Nothing) gameDecoder
                                        categoryDecoder
                                        ( JD.at [ "splitSetSegments" ] splitSetDecoder )

gameDecoder : JD.Decoder Timer.Game
gameDecoder = JD.map4 Timer.Game ( JD.at [ "splitSetGameID" ] <| JD.maybe JD.int )
                                 ( JD.at [ "splitSetGameData", "gameName" ] JD.string )
                                 ( JD.at [ "splitSetGameData", "gameIcon" ] <| JD.maybe JD.string )
                                 ( JD.at [ "splitSetGameData", "gameDefaultOffset" ] JD.int )

categoryDecoder : JD.Decoder Timer.Category
categoryDecoder = JD.map3 Timer.Category ( JD.at [ "splitSetCategoryID" ] <| JD.maybe JD.int )
                                         ( JD.at [ "splitSetCategoryData", "categoryName" ] JD.string )
                                         ( JD.at [ "splitSetCategoryData", "categoryOffset" ] JD.int )

splitSetDecoder : JD.Decoder Timer.SplitSet
splitSetDecoder = JD.map (Timer.SplitSet [] Nothing) <|
    JD.list <| JD.map (\s -> Timer.Split s Nothing Nothing) <|
        JD.map7 Timer.Segment ( JD.field "segmentID" <| JD.maybe JD.int )
                              ( JD.field "segmentName" JD.string )
                              ( JD.field "segmentIcon" <| JD.maybe JD.string )
                              ( JD.field "segmentPB" <| JD.maybe JD.int )
                              ( JD.field "segmentGold" <| JD.maybe JD.int )
                              ( JD.field "segmentAverage" <| JD.maybe JD.int )
                              ( JD.field "segmentWorst" <| JD.maybe JD.int )

clientStateRequestDecoder : JD.Decoder WebsocketMessage
clientStateRequestDecoder =
    checkTag "ClientStateRequest" <| JD.map ClientStateRequest ( JD.at [ "data", "contents" ] JD.int )

splitsControlDecoder : JD.Decoder WebsocketMessage
splitsControlDecoder = JD.oneOf [ decodeRemoteStartSplit, decodeRemoteUnsplit, decodeRemoteSkip, decodeRemoteStop, decodeRemoteReset ]

decodeRemoteStartSplit : JD.Decoder WebsocketMessage
decodeRemoteStartSplit = checkTag "RemoteControl" <| checkTag2 "RemoteStartSplit" (JD.map SplitsControl <| JD.map RemoteStartSplit (JD.at [ "data", "contents", "contents" ] JD.int ))

decodeRemoteUnsplit : JD.Decoder WebsocketMessage
decodeRemoteUnsplit = checkTag "RemoteControl" <| checkTag2 "RemoteUnsplit" (JD.map SplitsControl <| JD.succeed RemoteUnsplit)

decodeRemoteSkip : JD.Decoder WebsocketMessage
decodeRemoteSkip = checkTag "RemoteControl" <| checkTag2 "RemoteSkip" (JD.map SplitsControl <| JD.succeed RemoteSkip)

decodeRemoteStop : JD.Decoder WebsocketMessage
decodeRemoteStop = checkTag "RemoteControl" <| checkTag2 "RemoteStop" (JD.map SplitsControl <| JD.succeed RemoteStop)

decodeRemoteReset : JD.Decoder WebsocketMessage
decodeRemoteReset = checkTag "RemoteControl" <| checkTag2 "RemoteReset" (JD.map SplitsControl <| JD.succeed RemoteReset)

{- SUBSCRIPTIONS -}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ WS.subscriptions SocketOpened SocketNotOpened SocketReceived
              , T.every 4 Tick
              ]

{- VIEW -}
parseRun : Timer.Splits -> List Displayable
parseRun splits =
    let tracker = .runTracker splits
    in case tracker of
        Timer.SingleCategory r -> tabulateSplits splits r
        Timer.MultiCategory rs -> []

unitize : Int -> Displayable
unitize t_ =
    let t = abs t_
        sign = if t_ < 0 then "+" else "-"
        millis  = modBy 1000 t
        seconds = modBy 60 (t // 1000)
        minutes = modBy 60 (t // (1000 * 60))
        hours   = modBy 60 (t // (1000 * 60 * 60))
    in Displayable hours minutes seconds millis sign

tabulateSplits : Timer.Splits -> Timer.Run -> List Displayable
tabulateSplits s r =
    let startTime = case .runStarted s of
                        Nothing -> unitize 0
                        Just t -> unitize <| T.posixToMillis t
    in List.foldl (tabulateSplits_ startTime) [] (.previous <| .splits r)

tabulateSplits_ : Displayable -> Timer.Split -> List Displayable -> List Displayable
tabulateSplits_ subtrahend split ds = ds ++ [unitize <| T.posixToMillis <| (Maybe.withDefault (T.millisToPosix 0) <| .endTime split) ]

showD : Bool -> List (String, Bool) -> Displayable -> Html Msg
showD showSign classes d =
    let hoursEmpty   = .hours d == 0
        minutesEmpty = .minutes d == 0 && hoursEmpty
    in Html.span [ HA.classList classes ] [ Html.span [ HA.classList [("time-sign", True), ("time-show-sign", showSign)] ] [ Html.text <| .sign d ]
                                          , Html.span [ HA.classList [("time-hours", True), ("time-empty", hoursEmpty)] ] [ Html.text <| String.fromInt <| .hours d ]
                                          , Html.span [ HA.classList [("time-separator", True), ("time-hours-separator", True), ("time-empty", hoursEmpty)] ] [ Html.text ":" ]
                                          , Html.span [ HA.classList [("time-minutes", True), ("time-empty", minutesEmpty)] ] [ Html.text <| String.padLeft 2 '0' <| String.fromInt <| .minutes d ]
                                          , Html.span [ HA.classList [("time-minutes-nolead", True), ("time-empty", minutesEmpty)] ] [ Html.text <| String.fromInt <| .minutes d ]
                                          , Html.span [ HA.classList [("time-separator", True), ("time-minutes-separator", True), ("time-empty", minutesEmpty)] ] [ Html.text ":" ]
                                          , Html.span [ HA.classList [("time-seconds", True)] ] [ Html.text <| String.padLeft 2 '0' <| String.fromInt <| .seconds d ]
                                          , Html.span [ HA.classList [("time-seconds-nolead", True)] ] [ Html.text <| String.fromInt <| .seconds d ]
                                          , Html.span [ HA.classList [("time-separator", True), ("time-seconds-separator", True)] ] [ Html.text "." ]
                                          , Html.span [ HA.classList [("time-millis", True)] ] [ Html.text <| String.padLeft 2 '0' <| String.fromInt <| (.millis d) // 10 ]
                                          ]

mainTimer : Timer.Timer -> Html Msg
mainTimer timer =
    let splits = Timer.splitsFor <| timer
        currentTime_ = .currentTime splits
        currentTime = T.posixToMillis currentTime_
        runStarted  = T.posixToMillis <| Maybe.withDefault currentTime_ (.runStarted splits)
        diffTime = case timer of
                       Timer.Finished _ -> case Timer.runFor timer of
                                               Nothing -> -1 * (currentTime - runStarted) -- Required to make the function total; should be unreachable
                                               Just r  -> (T.posixToMillis <| Maybe.withDefault currentTime_ <| .runEnded r) - runStarted
                       _ -> currentTime - runStarted
    in Html.div [ HA.id "main-timer", HA.classList <| timeStatus timer ] [ showD False [] (unitize diffTime) ]

timeStatus : Timer.Timer -> List (String, Bool)
timeStatus timer =
    case Timer.runFor timer of
        Nothing -> [("neutral", True)] -- Multi-category run which contains no categories - prevents unformatted timer text
        Just r  ->
            let s = .splits r
                currentTime_ = T.posixToMillis <| .currentTime <| Timer.splitsFor timer
                currentTime = Maybe.withDefault currentTime_ <| Maybe.map (\t -> currentTime_ - T.posixToMillis t) (.runStarted r)
            in case .current s of
                Nothing -> case (List.reverse <| .previous s) of
                    [] -> [("neutral", True)] -- Run not yet started
                    x :: xs -> let (sum, single) = currentSum (List.reverse <| x :: xs)
                                   finalTime = Maybe.withDefault currentTime (Maybe.map2 (\t1 t2 -> (T.posixToMillis t1) - (T.posixToMillis t2)) (.runEnded r) (.runStarted r))
                               in case single of
                                   Just singleTime -> [ ("ahead", sum >= finalTime), ("behind", sum < finalTime)
                                                      , ("gaining", singleTime <= ((Maybe.withDefault 0 (Maybe.map T.posixToMillis <| .endTime x)) - (Maybe.withDefault 0 (Maybe.map T.posixToMillis <| .runStarted r))))
                                                      , ("losing", singleTime > ((Maybe.withDefault 0 (Maybe.map T.posixToMillis <| .endTime x)) - (Maybe.withDefault 0 (Maybe.map T.posixToMillis <| .runStarted r))))
                                                      ]
                                   Nothing -> [("ahead", True), ("gaining", True)] -- Impossible (except during multi-category runs in between categories or first run in a fresh category) (TODO)
                Just x -> let (sum, single) = currentSum ((.previous s) ++ [x])
                          in case single of
                              Just singleTime -> [ ("ahead", sum >= currentTime), ("behind", sum < currentTime)
                                                 , ("gaining", singleTime >= currentTime)
                                                 , ("losing", singleTime < currentTime)
                                                 ]
                              Nothing -> [ ("ahead", sum >= currentTime), ("behind", sum < currentTime)
                                         , ("gaining", sum <= ((Maybe.withDefault 0 (Maybe.map T.posixToMillis <| .endTime x)) - (Maybe.withDefault 0 (Maybe.map T.posixToMillis <| .runStarted r))))
                                         , ("losing", sum > ((Maybe.withDefault 0 (Maybe.map T.posixToMillis <| .endTime x)) - (Maybe.withDefault 0 (Maybe.map T.posixToMillis <| .runStarted r))))
                                         ]

currentSum : List Timer.Split -> (Int, Maybe Int)
currentSum splits = List.foldl currentSum_ (0, Nothing) splits

currentSum_ : Timer.Split -> (Int, Maybe Int) -> (Int, Maybe Int)
currentSum_ split (sum, _) =
    let next = Maybe.withDefault sum <| .pb <| .segment split
        single = Maybe.map (\x -> x - next) <| .pb <| .segment split
    in (next, single)

timerViewNormal : Int -> Timer.Timer -> Html Msg
timerViewNormal max timer =
    let splits = Timer.splitsFor timer in
        Html.div [ HA.id "timer-container" ]
                 [ Html.div [ HA.id "timer-title" ] [ Html.text <| .title splits ]
                 , Html.div [ HA.id "timer-subtitle" ] [ Html.text <| .subtitle splits ]
                 , splitsList max splits
                 , mainTimer timer
                 ]

splitHeaders : List (Html Msg)
splitHeaders =
    [ Html.div [ HA.classList [("segments-header", True)] ]
               [ Html.div [ HA.classList [("header", True),("pb", True)] ] [ Html.text "PB" ]
               , Html.div [ HA.classList [("header", True),("gold", True)] ] [ Html.text "Best" ]
               , Html.div [ HA.classList [("header", True),("average", True)] ] [ Html.text "Avg" ]
               , Html.div [ HA.classList [("header", True),("worst", True)] ] [ Html.text "Worst" ]
               , Html.div [ HA.classList [("header", True),("split", True)] ] [ Html.text "Split" ]

               , Html.div [ HA.classList [("header", True),("running", True),("pb", True)] ] [ Html.text "PB" ]
               , Html.div [ HA.classList [("header", True),("running", True),("gold", True)] ] [ Html.text "Best" ]
               , Html.div [ HA.classList [("header", True),("running", True),("average", True)] ] [ Html.text "Avg" ]
               , Html.div [ HA.classList [("header", True),("running", True),("worst", True)] ] [ Html.text "Worst" ]
               , Html.div [ HA.classList [("header", True),("running", True),("split", True)] ] [ Html.text "Split" ]

               , Html.div [ HA.classList [("header", True),("diff", True),("pb", True)] ] [ Html.text "+/- PB" ]
               , Html.div [ HA.classList [("header", True),("diff", True),("gold", True)] ] [ Html.text "+/- Best" ]
               , Html.div [ HA.classList [("header", True),("diff", True),("average", True)] ] [ Html.text "+/- Avg" ]
               , Html.div [ HA.classList [("header", True),("diff", True),("worst", True)] ] [ Html.text "+/- Worst" ]

               , Html.div [ HA.classList [("header", True),("running-diff", True),("pb", True)] ] [ Html.text "+/- PB" ]
               , Html.div [ HA.classList [("header", True),("running-diff", True),("gold", True)] ] [ Html.text "+/- Best" ]
               , Html.div [ HA.classList [("header", True),("running-diff", True),("average", True)] ] [ Html.text "+/- Avg" ]
               , Html.div [ HA.classList [("header", True),("running-diff", True),("worst", True)] ] [ Html.text "+/- Worst" ]
               ]
    ]

splitsList : Int -> Timer.Splits -> Html Msg
splitsList max splits =
    let currentTime = T.posixToMillis <| Maybe.withDefault (.currentTime splits) (.runStarted splits)
    in case .runTracker splits of
            Timer.SingleCategory r -> Html.div [ HA.id "splits-container" ] <| splitHeaders ++ (splitsList_ max <| Timer.aggregateRun currentTime r)
            Timer.MultiCategory rs ->
                let current = case .current rs of
                                  Nothing -> []
                                  Just r  -> [r]
                in Html.div [ HA.id "splits-container" ] <| splitHeaders -- TODO: This needs a specialized view
                    ++ (List.concatMap (splitsList_ max) <| List.map (Timer.aggregateRun currentTime) <| .previous rs)
                    ++ (List.concatMap (splitsList_ max) <| List.map (Timer.aggregateRun currentTime) current)
                    ++ (List.concatMap (splitsList_ max) <| List.map (Timer.aggregateRun currentTime) <| .upcoming rs)

splitsList_ : Int -> Timer.TimingData -> List (Html Msg)
splitsList_ max timing =
    let current_ = case (.current timing) of
                      Nothing -> []
                      Just t  -> [t]
        f s = List.map (\x -> (s, x))
        last = f "upcoming" <| List.take 1 <| List.reverse (.upcoming timing)
        pre1 = f "previous" <| List.reverse <| List.take 3 (List.reverse (.previous timing))
        current = f "current" <| current_
        upcoming = f "upcoming" <| List.take (max - (List.length (pre1 ++ last ++ current))) (List.reverse <| List.drop 1 <| List.reverse (.upcoming timing))
        pre = f "previous" <| List.reverse <| List.take (max - (List.length (pre1 ++ current ++ upcoming ++ last))) (List.drop 3 (List.reverse (.previous timing)))
        data = List.map (\x -> (1,x)) <| pre ++ pre1 ++ current ++ upcoming ++ last
        (_, divs) = List.foldl aggregateData (0, []) data
    in divs

aggregateData : (Int, (String, Timer.TimingDatum)) -> (Int, List (Html Msg)) -> (Int, List (Html Msg))
aggregateData (i, (tag, timing)) (j, hs) =
    let diff = diffDatum timing
        even = (modBy 2 (i + j)) == 0
        div = Html.div [ HA.classList ((.timingTags timing) ++ [("segment", True), (tag, True), ("even", even), ("odd", not even)]) ]
                       [ Html.div [ HA.class "segment-name" ] [ Html.text <| .name <| .segment timing ]
                       , splitSegment False ["pb"]      (.pb      <| .segment timing)
                       , splitSegment False ["gold"]    (.gold    <| .segment timing)
                       , splitSegment False ["average"] (.average <| .segment timing)
                       , splitSegment False ["worst"]   (.worst   <| .segment timing)
                       , splitSegment False ["split"]   (.singleTime timing)

                       , splitSegment False ["running", "pb"]      (.pb      <| .running timing)
                       , splitSegment False ["running", "gold"]    (.gold    <| .running timing)
                       , splitSegment False ["running", "average"] (.average <| .running timing)
                       , splitSegment False ["running", "worst"]   (.worst   <| .running timing)
                       , splitSegment False ["running", "split"]   (.currentSum timing)

                       , splitSegment True ["diff", "pb"]      (.pb      <| .segment diff)
                       , splitSegment True ["diff", "gold"]    (.gold    <| .segment diff)
                       , splitSegment True ["diff", "average"] (.average <| .segment diff)
                       , splitSegment True ["diff", "worst"]   (.worst   <| .segment diff)

                       , splitSegment True ["running-diff", "pb"]      (.pb      <| .running diff)
                       , splitSegment True ["running-diff", "gold"]    (.gold    <| .running diff)
                       , splitSegment True ["running-diff", "average"] (.average <| .running diff)
                       , splitSegment True ["running-diff", "worst"]   (.worst   <| .running diff)
                       ]
    in (i+j, hs ++ [div])

diffDatum : Timer.TimingDatum -> Timer.TimingDatum
diffDatum d =
    let s = .segment d
        r = .running d
        sdiff = \seg f g -> case f seg of Nothing -> Nothing
                                          Just a -> Maybe.map (\b -> a - b) (g d)
    in  { d | segment = { s | pb =      sdiff s .pb .singleTime
                            , gold =    sdiff s .gold .singleTime
                            , average = sdiff s .average .singleTime
                            , worst =   sdiff s .worst .singleTime
                        }
            , running = { r | pb =      sdiff r .pb .currentSum
                            , gold =    sdiff r .gold .currentSum
                            , average = sdiff r .average .currentSum
                            , worst =   sdiff r .worst .currentSum
                        }
        }

splitSegment : Bool -> List String -> Maybe Int -> Html Msg
splitSegment showSign tags i =
    let tags_ = List.map (\t -> (t, True)) tags
    in case i of
           Nothing -> Html.div [ HA.classList (tags_ ++ [("split-column", True), ("segment-time", True)]) ] [ Html.span [] [ Html.span [ HA.class "time-separator" ] [ Html.text "-" ] ] ]
           Just t  -> Html.div [ HA.classList (tags_ ++ [("split-column", True), ("segment-time", True)]) ] [ showD showSign [] <| unitize t ]

timerView : Model -> Html Msg
timerView model = timerViewNormal (.maxSegmentsShown model) (.timer model)

view : Model -> Html Msg
view model =
    Html.div [ HA.id "main-container" ]
             [ Html.div [ HA.id "app-top" ] [ timerView model ] ]
