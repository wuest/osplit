module Main exposing ( main )

import Browser
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Encode as JE
import Json.Decode as JD
import Dict as Dict

import Types exposing (Msg(..), Socket, WebsocketMessage(..), SplitsMessage(..), TimeSyncResponse, ConfigStore)
import Websocket as WS
import Timer as Timer
import Timer.Types as TT

import Debug as D

main = Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

{- MODEL -}

type alias Model = { socket       : Maybe Socket
                   , timer        : Timer.Timer
                   , timeOffset   : Int
                   , activePane   : TimerPane
                   , menuPane     : MenuPane
                   , gameList     : List TT.Game
                   , categoryList : List TT.Category
                   , configStore  : ConfigStore
                   }

type TimerPane = Splits | Menu

type MenuPane = MainMenu | GameList | CategoryList

init : String -> (Model, Cmd Msg)
init url = ( { socket = Nothing
             , timer = Timer.empty
             , timeOffset = 0
             , activePane = Splits
             , menuPane = MainMenu
             , gameList = []
             , categoryList = []
             , configStore = Dict.empty
             }
           , WS.open url
           )

send : Maybe Socket -> JE.Value -> Cmd msg
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
            ({ model | socket = Just newsocket }, WS.send newsocket joinAnnounce)
        SocketNotOpened ->
            (model, Cmd.none)
        SocketReceived data ->
            processData model data
        SyncTime _ ->
            (model, send (.socket model) (Timer.syncRequest (.timer model)))
        CloseSplits ->
            ({ model | activePane = Splits, timer = Timer.empty }, broadcastIntent (.socket model) msg)
        LoadSplits _ ->
            ({ model | activePane = Splits }, broadcastIntent (.socket model) msg)
        ToggleMainMenu ->
            (toggleMainMenu model, Cmd.none)
        ListGames ->
            ({ model | menuPane = GameList, gameList = [] }, broadcastIntent (.socket model) msg)
        ListCategories _ ->
            ({ model | menuPane = CategoryList, categoryList = [] }, broadcastIntent (.socket model) msg)
        StartSplit i -> -- this should probably be extracted
            let t = .timer model in
                if Timer.isCompleted (.timer model)
                then
                    ({ model | timer = Timer.reset t }, broadcastIntent (.socket model) (Finish <| .timer model)) -- TODO SAVE HERE
                else
                    ({ model | timer = Timer.update msg t }, broadcastIntent (.socket model) msg)
        _ ->
            ({ model | timer = Timer.update msg <| .timer model }, broadcastIntent (.socket model) msg)

toggleMainMenu : Model -> Model
toggleMainMenu model = case .activePane model of
    Splits -> { model | activePane = Menu, menuPane = MainMenu }
    Menu   -> { model | activePane = Splits }

broadcastIntent : Maybe Socket -> Msg -> Cmd Msg
broadcastIntent ms msg = case msg of
    -- Splits controls
    StartSplit t     -> send ms <| startSplitAnnounce t
    Finish timer     -> send ms <| finishAnnounce timer
    Unsplit t        -> send ms <| unsplitAnnounce t
    Skip t           -> send ms <| skipAnnounce t
    Stop t           -> send ms <| stopAnnounce t
    Reset timer      -> send ms <| resetAnnounce timer
    -- Menu/navigation
    CloseSplits      -> send ms    closeSplitsRequest
    ListGames        -> send ms    gameListRequest
    ListCategories g -> case g of
        Nothing -> Cmd.none
        Just g_ -> send ms <| categoryListRequest g_
    -- Load/editmanipulate splits
    LoadSplits c     -> case c of
        Nothing -> Cmd.none
        Just c_ -> send ms <| splitsLoadRequest c_
    -- Anything not explicitly matched can be safely ignored
    _                -> Cmd.none

joinAnnounce : JE.Value
joinAnnounce = JE.object [ ( "tag", JE.string "NewClient" )
                         , ( "contents", JE.null )
                         ]

startSplitAnnounce : Int -> JE.Value
startSplitAnnounce t = JE.object [ ( "tag", JE.string "TimerControl" )
                                 , ( "contents", JE.object [ ( "tag", JE.string "RemoteStartSplit" )
                                                           , ( "contents", JE.int t )
                                                           ]
                                   )
                                 ]

finishAnnounce : Timer.Timer -> JE.Value
finishAnnounce t = JE.object [ ( "tag", JE.string "TimerControl" )
                             , ( "contents", JE.object [ ( "tag", JE.string "RemoteFinish" )
                                                       , ( "contents", Timer.toJSON t )
                                                       ]
                               )
                             ]

skipAnnounce : Int -> JE.Value
skipAnnounce t = JE.object [ ( "tag", JE.string "TimerControl" )
                           , ( "contents", JE.object [ ( "tag", JE.string "RemoteSkip" )
                                                     , ( "contents", JE.int t )
                                                     ]
                             )
                           ]

unsplitAnnounce : Int -> JE.Value
unsplitAnnounce t = JE.object [ ( "tag", JE.string "TimerControl" )
                              , ( "contents", JE.object [ ( "tag", JE.string "RemoteUnsplit" )
                                                        , ( "contents", JE.int t )
                                                        ]
                                )
                              ]

stopAnnounce : Int -> JE.Value
stopAnnounce t = JE.object [ ( "tag", JE.string "TimerControl" )
                           , ( "contents", JE.object [ ( "tag", JE.string "RemoteStop" )
                                                     , ( "contents", JE.int t )
                                                     ]
                             )
                           ]

resetAnnounce : Timer.Timer -> JE.Value
resetAnnounce t = JE.object [ ( "tag", JE.string "TimerControl" )
                            , ( "contents", JE.object [ ( "tag", JE.string "RemoteReset" )
                                                      , ( "contents", Timer.toJSON t )
                                                      ]
                              )
                            ]

closeSplitsRequest : JE.Value
closeSplitsRequest = JE.object [ ( "tag", JE.string "Menu" )
                               , ( "contents", JE.object [ ( "tag", JE.string "MenuCloseSplits" ) ] )
                               ]

gameListRequest : JE.Value
gameListRequest = JE.object [ ( "tag", JE.string "Menu" )
                            , ( "contents", JE.object [ ( "tag", JE.string "MenuGames" ) ] )
                            ]

categoryListRequest : Int -> JE.Value
categoryListRequest game = JE.object [ ( "tag", JE.string "Menu" )
                                     , ( "contents", JE.object [ ( "tag", JE.string "MenuCategories")
                                                               , ( "contents", JE.int game )
                                                               ]
                                       )
                                     ]

splitsLoadRequest : Int -> JE.Value
splitsLoadRequest category = JE.object [ ( "tag", JE.string "Menu" )
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
        Ok (TimeSync response)        -> ( processTimeSync model response, Cmd.none )
        Ok (SplitsControl ctl)        -> ( { model | timer = Timer.update (remoteControl ctl) (.timer model) }, Cmd.none )
        Ok (UnloadSplits)             -> ( { model | timer = Timer.empty }, Cmd.none )
        Ok (FetchedGameList games)    -> ( { model | gameList = games }, Cmd.none )
        Ok (FetchedCategoryList cats) -> ( { model | categoryList = cats }, Cmd.none )
        Ok (FetchedSplits Nothing)    -> ( model, Cmd.none ) -- TODO: ERROR HANDLE THIS
        Ok (FetchedSplits (Just s))   -> ( { model | timer = Timer.load s }, Cmd.none )
        Ok (ConfigStoreSet k v)       -> ( { model | configStore = Dict.insert k v (.configStore model) }, broadcastIntent (.socket model) (LoadSplits (String.toInt v) ) ) -- TODO: CURRENTLY this fires only at initialization.  This is a bad assumption.  Rework this.
        x                             -> ( model, Cmd.none ) -- TODO: ESPECIALLY ERROR HANDLE THIS

processIncomingEvent : JE.Value -> Result JD.Error WebsocketMessage
processIncomingEvent =
        JD.decodeValue <| JD.oneOf [ timeSyncResponseDecoder
                                   , clientStateRequestDecoder
                                   , splitsControlDecoder
                                   , gameListDecoder
                                   , categoryListDecoder
                                   , splitsDecoder
                                   , closeSplitsDecoder
                                   , configStoreDecoder
                                   ]

closeSplitsDecoder : JD.Decoder WebsocketMessage
closeSplitsDecoder = checkTag "CloseSplits" <| JD.map (\_ -> UnloadSplits) ( JD.at [ "data", "tag" ] JD.string )

gameListDecoder : JD.Decoder WebsocketMessage
gameListDecoder =
    checkTag "GameList" <| JD.map FetchedGameList
        ( JD.at [ "data", "contents" ] <| JD.list
            ( JD.map4 TT.Game ( JD.at [ "gameID" ] <| JD.maybe JD.int )
                              ( JD.at [ "gameData", "gameName" ] JD.string )
                              ( JD.at [ "gameData", "gameIcon" ] <| JD.maybe JD.string )
                              ( JD.at [ "gameData", "gameDefaultOffset" ] JD.int )
            )
        )

categoryListDecoder : JD.Decoder WebsocketMessage
categoryListDecoder =
    checkTag "CategoryList" <| JD.map FetchedCategoryList
        ( JD.at [ "data", "contents" ] <| JD.list
            ( JD.map3 TT.Category ( JD.at [ "categoryID" ] <| JD.maybe JD.int )
                                  ( JD.at [ "categoryData", "categoryName" ] JD.string )
                                  ( JD.at [ "categoryData", "categoryOffset" ] JD.int )
            )
        )

configStoreDecoder : JD.Decoder WebsocketMessage
configStoreDecoder =
    checkTag "ConfigStore" <| JD.map2 ConfigStoreSet ( JD.at [ "data", "contents" ] <| JD.index 0 JD.string )
                                                     ( JD.at [ "data", "contents" ] <| JD.index 1 JD.string )

splitsDecoder : JD.Decoder WebsocketMessage
splitsDecoder =
    checkTag "SplitsRefresh" <| JD.map FetchedSplits
        ( JD.at [ "data", "contents" ] <| JD.nullable
            ( JD.map3 TT.SplitsSpec newGameDecoder newCategoryDecoder newSegmentListDecoder )
        )

newGameDecoder : JD.Decoder TT.Game
newGameDecoder =
    JD.map4 TT.Game ( JD.at [ "splitSetGameID" ] <| JD.maybe JD.int )
                    ( JD.at [ "splitSetGameData", "gameName" ] JD.string )
                    ( JD.at [ "splitSetGameData", "gameIcon" ] <| JD.nullable JD.string )
                    ( JD.at [ "splitSetGameData", "gameDefaultOffset" ] JD.int )

newCategoryDecoder : JD.Decoder TT.Category
newCategoryDecoder =
    JD.map3 TT.Category ( JD.at [ "splitSetCategoryID" ] <| JD.maybe JD.int )
                        ( JD.at [ "splitSetCategoryData", "categoryName" ] JD.string )
                        ( JD.at [ "splitSetCategoryData", "categoryOffset" ] JD.int )

newSegmentListDecoder : JD.Decoder TT.SplitSet
newSegmentListDecoder = JD.at [ "splitSetSegments" ] <| JD.list <|
    JD.map (\x -> TT.Split x Nothing Nothing) ( newSegmentDecoder )

newSegmentDecoder : JD.Decoder TT.Segment
newSegmentDecoder =
    JD.map7 TT.Segment ( JD.at [ "segmentID" ] <| JD.maybe JD.int )
                       ( JD.at [ "segmentName" ] JD.string )
                       ( JD.at [ "segmentIcon" ] <| JD.nullable JD.string )
                       ( JD.at [ "segmentPB" ] <| JD.nullable JD.int )
                       ( JD.at [ "segmentGold" ] <| JD.nullable JD.int )
                       ( JD.at [ "segmentAverage" ] <| JD.nullable JD.int )
                       ( JD.at [ "segmentWorst" ] <| JD.nullable JD.int )

clientStateRequestDecoder : JD.Decoder WebsocketMessage
clientStateRequestDecoder =
    checkTag "ClientStateRequest" <| JD.map ClientStateRequest ( JD.at [ "data", "contents" ] JD.int )

timeSyncResponseDecoder : JD.Decoder WebsocketMessage
timeSyncResponseDecoder =
    JD.map TimeSync <|
        JD.map2 TimeSyncResponse ( JD.at [ "data", "contents", "currentTime" ] JD.int )
                                 ( JD.at [ "data", "contents", "previousOffset" ] JD.int )

splitsControlDecoder : JD.Decoder WebsocketMessage
splitsControlDecoder = JD.oneOf [ decodeRemoteStartSplit, decodeRemoteUnsplit, decodeRemoteSkip, decodeRemoteStop, decodeRemoteReset ]

decodeRemoteStartSplit : JD.Decoder WebsocketMessage
decodeRemoteStartSplit = checkTag "RemoteControl" <| checkTag2 "RemoteStartSplit" (JD.map SplitsControl (JD.map RemoteStartSplit <| JD.at [ "data", "contents", "contents" ] JD.int))

decodeRemoteUnsplit : JD.Decoder WebsocketMessage
decodeRemoteUnsplit = checkTag "RemoteControl" <| checkTag2 "RemoteUnsplit" (JD.map SplitsControl (JD.map RemoteUnsplit <| JD.at [ "data", "contents", "contents" ] JD.int))

decodeRemoteSkip : JD.Decoder WebsocketMessage
decodeRemoteSkip = checkTag "RemoteControl" <| checkTag2 "RemoteSkip" (JD.map SplitsControl (JD.map RemoteSkip <| JD.at [ "data", "contents", "contents" ] JD.int))

decodeRemoteStop : JD.Decoder WebsocketMessage
decodeRemoteStop = checkTag "RemoteControl" <| checkTag2 "RemoteStop" (JD.map SplitsControl (JD.map RemoteStop <| JD.at [ "data", "contents", "contents" ] JD.int))

decodeRemoteReset : JD.Decoder WebsocketMessage
decodeRemoteReset = checkTag "RemoteControl" <| checkTag2 "RemoteReset" (JD.map SplitsControl (JD.succeed RemoteReset))

processTimeSync : Model -> TimeSyncResponse -> Model
processTimeSync model response = { model | timeOffset = Timer.processSyncResponse (.timer model) response }

remoteControl : SplitsMessage -> Msg
remoteControl s =
    case s of
        RemoteStartSplit t -> StartSplit t
        RemoteUnsplit t    -> Unsplit t
        RemoteSkip t       -> Skip t
        RemoteStop t       -> Stop t
        RemoteReset        -> Reset Timer.empty

{- SUBSCRIPTIONS -}
subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch [ WS.subscriptions
                            , Timer.subscriptions
                            ]

{- VIEW -}
li : String -> Html Msg
li string = Html.li [] [Html.text string]

timerView : Model -> Html Msg
timerView model = case .activePane model of
    Splits -> Html.div [ HA.id "timer-top" ]
                       [ Timer.view <| .timer model ]
    Menu -> Html.div [ HA.id "timer-top" ]
                     [ Timer.edit <| .timer model ]

menuView : Model -> Html Msg
menuView model = case .menuPane model of
    MainMenu ->
        Html.div [ HA.id "menu-top" ]
                 [ Html.div [ HA.class "menu-button"
                            , HE.onClick ListGames
                            ]
                            [ Html.text "Load Game" ]
                 , Html.div [ HA.class "menu-button"
                            , HE.onClick CloseSplits
                            ]
                            [ Html.text "Close Game" ]
                 ]
    GameList ->
        Html.div [ HA.id "menu-top" ]
                 [ Html.div [ ]
                            (List.map gameListView <| .gameList model)
                 ]
    CategoryList ->
        Html.div [ HA.id "menu-top" ]
                 [ Html.div [ ]
                            (List.map categoryListView <| .categoryList model)
                 ]

gameListView : TT.Game -> Html Msg
gameListView game =
    Html.div [ HA.class "menu-button"
             , HE.onClick <| ListCategories (.entityID game)
             ]
             [ Html.text <| .name game ]

categoryListView : TT.Category -> Html Msg
categoryListView cat =
    Html.div [ HA.class "menu-button"
             , HE.onClick <| LoadSplits (.entityID cat)
             ]
             [ Html.text <| .name cat ]

menuButton : Html Msg
menuButton = Html.div [ HA.id "menu-container" ]
                      [ Html.div [ HA.class "menu-button"
                                 , HE.onClick ToggleMainMenu
                                 ]
                                 [ Html.text "☰" ]
                      ]

view : Model -> Html Msg
view model = case .activePane model of
    Splits ->
        Html.div [ HA.id "main-container" ]
                 [ Html.div [ HA.id "app-top" ]
                            [ timerView model
                            ]
                 , menuButton
                 ]
    Menu ->
        Html.div [ HA.id "main-container" ]
                 [ Html.div [ HA.id "app-top" ]
                            [ menuView model
                            ]
                 , menuButton
                 ]
