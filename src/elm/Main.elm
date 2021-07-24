module Main exposing ( main )

import Browser
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Encode as JE
import Json.Decode as JD
import Dict as Dict

{- INPUT HANDLING -}
import Keyboard as KB
import Gamepad exposing (Gamepad)
import Gamepad.Advanced exposing (Blob, UserMappings)
import GamepadPort as GamepadPort

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

type alias MultiCategoryInfo = { name : String
                               , entityID : Int
                               }

type alias FullGameList = { gameList : List Timer.Game
                          , multiCategoryList : List MultiCategoryInfo
                          }

type Msg = OpenSocket String
         | SendSocket JE.Value
         | SocketOpened WS.Socket
         | SocketNotOpened JD.Error
         | SocketReceived JE.Value
         -- User interaction
         | KeyboardEvent KB.Msg
         | GamepadAnimationFrame Blob
         | GamepadRemappingTool Gamepad.Advanced.Msg
         | GamepadFrame Blob
         | GamepadMappingLoad String
         -- UI
         | ToggleMainMenu
         | InputConfig
         | RemapGamepadToggle
         | ListGames
         | ListCategories (Maybe Int)
         | LoadMultiCategory Int
         | LoadSplits (Maybe Int)
         | CloseSplits
         | EditSplits
         | EditSplitsSave
         | EditSplitsCancel
         | UpdateMultiCategory Bool
         | UpdateSegmentName Int String
         | UpdateTitle String
         | UpdateSubtitle String
         | UpdateSelectedGame Int String
         | AddSegment
         | AddGame
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
                      | FetchedGameList FullGameList
                      | FetchedCategoryList (List Timer.Category)
                      | FullCategoryList (List Timer.RunSpec)
                      | FetchedSplits Timer.SplitsSpec
                      | ConfigStoreSet CSKey CSVal

type SplitsMessage = RemoteStartSplit Int
                   | RemoteUnsplit
                   | RemoteSkip
                   | RemoteStop
                   | RemoteReset

type alias Model = { socket            : Maybe WS.Socket
                   , timer             : Timer.Timer
                   , backupTimer       : Maybe Timer.Timer
                   , splitsMode        : SplitsMode
                   , menu              : MenuStatus
                   , gameList          : List Timer.Game
                   , multiCategoryList : List MultiCategoryInfo
                   , categoryList      : List Timer.Category
                   , keyboardStatus    : List KB.Key
                   , keyboardMap       : KeyboardMapping
                   , gamepadMap        : GamepadMapping
                   , gamepadState      : GamepadState
                   , userMappings      : UserMappings
                   , configStore       : ConfigStore
                   , maxSegmentsShown  : Int
                   , editMulticategory : Bool
                   , fullCategoryList  : List Timer.RunSpec
                   }

type GamepadState = Uninitialized
                  | RemappingGamepad Gamepad.Advanced.Model
                  | ActiveGamepad Blob

type MenuStatus = MenuHidden | MenuVisible MenuNav

type MenuNav = MainMenu | Config | GameList | CategoryList

type alias FunctionMap = (UIInteraction, (Timer.Timer -> Msg))

type alias FunctionDescMap = List (UIInteraction, String)

type alias KeyboardMapping = List (UIInteraction, KB.Key)

type alias GamepadMapping = List (UIInteraction, Gamepad.Digital)

type SplitsMode = NormalSplitsView | EditSplitsView

type UIInteraction = UISplit | UIUnsplit | UISkip | UIReset | UIMenu

main = Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

init : String -> (Model, Cmd Msg)
init url = ( { socket = Nothing
             , timer = Timer.empty
             , backupTimer = Nothing
             , splitsMode = NormalSplitsView
             , menu = MenuHidden
             , gameList = []
             , multiCategoryList = []
             , categoryList = []
             , keyboardStatus = []
             , keyboardMap = defaultKeyboardMap
             , gamepadMap = defaultGamepadMap
             , gamepadState = Uninitialized
             , userMappings = Gamepad.Advanced.emptyUserMappings
             , configStore = Dict.empty
             , maxSegmentsShown = 10
             , editMulticategory = False
             , fullCategoryList = []
             }
           , Cmd.batch [WS.open url, GamepadPort.load]
           )

send : Maybe WS.Socket -> JE.Value -> Cmd msg
send ms v = case ms of
    Nothing -> Cmd.none
    Just s -> WS.send s v

controlDescMap : FunctionDescMap
controlDescMap = [ (UISplit,   "Split")
                 , (UIUnsplit, "Unsplit")
                 , (UISkip,    "Skip Segment")
                 , (UIReset,   "Reset Run")
                 , (UIMenu,    "Toggle Menu")
                 ]

defaultKeyboardMap : KeyboardMapping
defaultKeyboardMap = [ (UISplit,   KB.Spacebar)
                     , (UIUnsplit, KB.ArrowLeft)
                     , (UISkip,    KB.ArrowRight)
                     , (UIReset,   KB.Character "R")
                     , (UIMenu,    KB.Escape)
                     ]

defaultGamepadMap : GamepadMapping
defaultGamepadMap = [ (UISplit, Gamepad.A)
                    , (UISkip,  Gamepad.X)
                    , (UIReset, Gamepad.Y)
                    ]

fetchControl : UIInteraction -> Timer.Timer -> Msg
fetchControl u = case u of
    UISplit   -> StartSplit << Just << .currentTime << Timer.splitsFor
    UIUnsplit -> const Unsplit
    UISkip    -> const Skip
    UIReset   -> Reset
    UIMenu    -> const ToggleMainMenu

allMappableControls : List ( String, Gamepad.Digital )
allMappableControls =
    [ ( "Split", Gamepad.A )
    , ( "Unsplit", Gamepad.B )
    , ( "Skip Segment", Gamepad.X )
    , ( "Reset Run", Gamepad.Y )
    ]

controlNames : List ( Gamepad.Digital, String )
controlNames =
    [ ( Gamepad.A, "Button A / Cross" )
    , ( Gamepad.B, "Button B / Circle" )
    , ( Gamepad.X, "Button X / Square" )
    , ( Gamepad.Y, "Button Y / Triangle" )
    , ( Gamepad.Start, "Button Start" )
    , ( Gamepad.Back, "Button Back / Select" )
    , ( Gamepad.Home, "Logo / Home / Guide" )
    , ( Gamepad.LeftStickLeft, "Left Stick: Push Left" )
    , ( Gamepad.LeftStickRight, "Left Stick: Push Right" )
    , ( Gamepad.LeftStickUp, "Left Stick: Push Up" )
    , ( Gamepad.LeftStickDown, "Left Stick: Push Down" )
    , ( Gamepad.LeftStickPress, "Left Stick: Click" )
    , ( Gamepad.LeftBumper, "Left Bumper Button" )
    , ( Gamepad.LeftTrigger, "Left Trigger / Left Analog Lever" )
    , ( Gamepad.RightStickLeft, "Right Stick: Push Left" )
    , ( Gamepad.RightStickRight, "Right Stick: Push Right" )
    , ( Gamepad.RightStickUp, "Right Stick: Push Up" )
    , ( Gamepad.RightStickDown, "Right Stick: Push Down" )
    , ( Gamepad.RightStickPress, "Right Stick: Click" )
    , ( Gamepad.RightBumper, "Right Bumper Button" )
    , ( Gamepad.RightTrigger, "Right Trigger / Right Analog Lever" )
    , ( Gamepad.DpadUp, "Directional Pad Up" )
    , ( Gamepad.DpadDown, "Directional Pad Down" )
    , ( Gamepad.DpadLeft, "Directional Pad Left" )
    , ( Gamepad.DpadRight, "Directional Pad Right" )
    ]

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
        {- User Interaction -}
        KeyboardEvent keys ->
            processKeyboardEvent model keys
        ToggleMainMenu -> case .menu model of
            MenuVisible _ -> ({ model | menu = MenuHidden }, Cmd.none)
            MenuHidden -> ({ model | menu = MenuVisible MainMenu }, Cmd.none)
        {- Timer functions -}
        Tick t ->
            ({ model | timer = Timer.setTime (.timer model) t }, Cmd.none)
        StartSplit _ ->
            timerControl True model msg
        Reset _ ->
            timerControl True model msg
        Unsplit ->
            timerControl True model msg
        Skip ->
            timerControl True model msg
        CloseSplits ->
            ({ model | menu = MenuHidden, timer = Timer.empty }, broadcastIntent (.socket model) msg)
        EditSplits ->
            ({ model | backupTimer = Just <| Timer.reset <| .timer model, timer = Timer.reset <| .timer model, menu = MenuHidden, splitsMode = EditSplitsView }, Cmd.none)
        EditSplitsCancel ->
            ({ model | timer = Maybe.withDefault Timer.empty <| .backupTimer model, menu = MenuHidden, splitsMode = NormalSplitsView }, Cmd.none) -- backupTimer *SHOULD* not ever be Nothing in this case
        UpdateSegmentName i n ->
            ({ model | timer = updateSegmentName i n <| .timer model }, Cmd.none)
        UpdateSelectedGame i c ->
            ({ model | timer = updateSelectedCategory (.fullCategoryList model) i (Maybe.withDefault -1 <| String.toInt c) (.timer model) }, Cmd.none)
        UpdateTitle n ->
            ({ model | timer = updateTitle n <| .timer model }, Cmd.none)
        UpdateSubtitle n ->
            ({ model | timer = updateSubtitle n <| .timer model }, Cmd.none)
        UpdateMultiCategory m -> -- TODO this should really just be empty always when we're editing anyway...
            let timerContainer = if m then Timer.emptyMulti else Timer.empty
            in ({ model | editMulticategory = m, timer = timerContainer }, send (.socket model) fullCategoryListRequestJSON)
        AddSegment  ->
            ({ model | timer = addSegment <| .timer model }, Cmd.none)
        AddGame ->
            ({ model | timer = addGame <| .timer model }, Cmd.none)
        EditSplitsSave ->
            (model, splitsSave (.socket model) (Timer.splitsFor <| .timer model))
        ListGames ->
            ({ model | menu = MenuVisible GameList, gameList = [] }, broadcastIntent (.socket model) msg)
        ListCategories _ ->
            ({ model | menu = MenuVisible CategoryList, categoryList = [] }, broadcastIntent (.socket model) msg)
        LoadSplits _ ->
            ({ model | menu = MenuHidden }, broadcastIntent (.socket model) msg)
        LoadMultiCategory _ ->
            ({ model | menu = MenuHidden }, broadcastIntent (.socket model) msg)
        InputConfig ->
            ({ model | menu = MenuVisible Config }, Cmd.none)
        RemapGamepadToggle ->
            let gp = case .gamepadState model of
                         RemappingGamepad _ -> Uninitialized
                         _ -> RemappingGamepad (Gamepad.Advanced.init allMappableControls)
            in ({ model | gamepadState = gp }, Cmd.none)
        GamepadMappingLoad serialized ->
            ({ model | userMappings = GamepadPort.fromString serialized (.userMappings model) }, Cmd.none)
        GamepadRemappingTool remap ->
            remapGamepad model remap
        GamepadFrame blob ->
            processGamepadEvent model blob
        {- Blackhole everything else -}
        _ -> (model, Cmd.none)

timerControl : Bool -> Model -> Msg -> (Model, Cmd Msg)
timerControl broadcast model msg =
    case .splitsMode model of
        EditSplitsView -> (model, Cmd.none)
        NormalSplitsView ->
            let msg_ = if broadcast then broadcastIntent (.socket model) msg else Cmd.none
            in case msg of
               StartSplit t ->
                   let offsetTime = Maybe.map (\time -> T.millisToPosix <| (T.posixToMillis time) - (Maybe.withDefault 0 (Maybe.map T.posixToMillis <| .runStarted <| Timer.splitsFor <| .timer model))) t
                       msg2 = if broadcast then broadcastIntent (.socket model) (StartSplit offsetTime) else Cmd.none
                   in case .timer model of
                          Timer.Running _  -> ({ model | timer = Timer.split t <| .timer model }, msg2)
                          Timer.Finished _ -> ({ model | timer = Timer.reset <| .timer model }, send (.socket model) (finishSplitsRequestJSON <| .timer model))
                          _                -> ({ model | timer = Timer.start t <| .timer model }, msg2)
               Reset _ ->
                   ({ model | timer = Timer.reset <| .timer model }, msg_)
               Unsplit ->
                   ({ model | timer = Timer.unsplit <| .timer model }, msg_)
               Skip ->
                   ({ model | timer = Timer.skip <| .timer model }, msg_)
               _ -> (model, Cmd.none)

{- TODO Adapted from Gamepad.elm example -}
remapGamepad : Model -> Gamepad.Advanced.Msg -> ( Model, Cmd Msg )
remapGamepad model remapMsg =
    case .gamepadState model of
        RemappingGamepad remapModelOld ->
            let ( remapModelNew, maybeUpdateUserMappings ) = Gamepad.Advanced.update remapMsg remapModelOld
            in remapGamepad_ maybeUpdateUserMappings { model | gamepadState = RemappingGamepad remapModelNew }
        _ -> (model, Cmd.none)


remapGamepad_ : Maybe (UserMappings -> UserMappings) -> Model -> ( Model, Cmd Msg )
remapGamepad_ maybeUpdateUserMappings model =
    case maybeUpdateUserMappings of
        -- Gamepad.Advanced.update didn't provide any function to update user mappings
        Nothing -> (model, Cmd.none)

        -- Gamepad.Advanced.update gave us a function to update user mappings, let's do it!
        Just updateMappings ->
            let newUserMappings = updateMappings (.userMappings model)
                newModel = { model | userMappings = newUserMappings }
                cmd =
                    if newUserMappings == (.userMappings model) then
                        -- userMappings didn't change in any meaningful way,
                        -- no need to change the URL.
                        Cmd.none
                    else
                        -- userMappings changed, let's "save" it in the URL!
                        GamepadPort.saveMappings <| Gamepad.Advanced.userMappingsToString newUserMappings
            in
            ( newModel, cmd )
{- end TODO Adapted from Gamepad.elm example -}

updateSegmentName : Int -> String -> Timer.Timer -> Timer.Timer
updateSegmentName i n = Timer.mapT <| Timer.mapR (updateSegmentName_ i n)

updateSegmentName_ : Int -> String -> Timer.Run -> Timer.Run
updateSegmentName_ i n run =
    let s = .splits run
    in { run | splits = { s | upcoming = List.foldl (\(j, split) splitlist -> if i == j
                                                                              then let seg = .segment split in splitlist ++ [{ split | segment = { seg | name = n } }]
                                                                              else splitlist ++ [split]
                                                    ) [] <| List.indexedMap Tuple.pair <| (.upcoming s) } }

updateSelectedCategory : List Timer.RunSpec -> Int -> Int -> Timer.Timer -> Timer.Timer
updateSelectedCategory rss i c =
        let cs = List.foldl (\spec categories ->
                                case spec of
                                    Timer.SingleCategorySpec r -> (.category r) :: categories
                                    Timer.MultiCategorySpec _ rs -> (List.map .category rs) ++ categories
                            ) [] rss
        in Timer.mapT (updateSelectedCategory_ cs i c)

updateSelectedCategory_ : List Timer.Category -> Int -> Int -> Timer.Splits -> Timer.Splits
updateSelectedCategory_ cs i c s =
    case .runTracker s of
        Timer.SingleCategory _ -> s
        Timer.MultiCategory rs ->
            let r = .upcoming rs
                m = Timer.MultiCategory <| { rs | upcoming = List.foldl (\(j, run) runList -> if i == j
                                                                                              then
                                                                                                  case List.head <| List.filter (\cat -> (.entityID cat) == (Just c)) cs
                                                                                                      of Nothing -> runList ++ [run]
                                                                                                         Just c_ -> runList ++ [{ run | category = c_ }]
                                                                                              else runList ++ [run]
                                                                        ) [] <| List.indexedMap Tuple.pair r
                                           }
            in { s | runTracker = m }

addSegment : Timer.Timer -> Timer.Timer
addSegment = Timer.mapT <| Timer.mapR addSegment_

addSegment_ : Timer.Run -> Timer.Run
addSegment_ run =
    let s = .splits run
        newSegment = Timer.emptySegment
        newSplit = [{ segment = { newSegment | name = "(New Segment)" }, endTime = Nothing, segmentTime = Nothing }]
    in { run | splits = { s | upcoming = (.upcoming s) ++ newSplit } }

addGame : Timer.Timer -> Timer.Timer
addGame = Timer.mapT addGame_

addGame_ : Timer.Splits -> Timer.Splits
addGame_ s =
    case .runTracker s of
        Timer.MultiCategory rt ->
            let newRun = [Timer.noRun]
            in { s | runTracker = Timer.MultiCategory <| { rt | upcoming = (.upcoming rt) ++ newRun } }
        _ -> s -- This should be unreachable, but is required in order to make this function total.

updateTitle : String -> Timer.Timer -> Timer.Timer
updateTitle n = Timer.mapT (updateTitle_ n)

updateTitle_ : String -> Timer.Splits -> Timer.Splits
updateTitle_ name splits = { splits | title = name }

updateSubtitle : String -> Timer.Timer -> Timer.Timer
updateSubtitle n = Timer.mapT (updateSubtitle_ n)

updateSubtitle_ : String -> Timer.Splits -> Timer.Splits
updateSubtitle_ name splits = { splits | subtitle = name }

processKeyboardEvent : Model -> KB.Msg -> (Model, Cmd Msg)
processKeyboardEvent model keyMsg =
    let f = (\key ms -> ms ++ (List.map (\(_,val) -> val) <| List.filter (\(kb,kbm) -> kb == key && (kbm == ToggleMainMenu || (.menu model) == MenuHidden)) (List.map (\(kbf,kbm) -> (kbm, (fetchControl kbf) (.timer model))) (.keyboardMap model))))
        keys = KB.update keyMsg (.keyboardStatus model)
        diff = List.filter (\key -> not <| List.member key (.keyboardStatus model)) keys
        msgs = List.foldl f [] diff
        (m_, c_) = List.foldl (\msg (model_, cmds) -> (\(v,c) -> (v, cmds ++ [c])) <| update msg model_) ({ model | keyboardStatus = keys }, []) msgs
    in  (m_, Cmd.batch c_)

processGamepadEvent : Model -> Blob -> (Model, Cmd Msg)
processGamepadEvent model blob =
    let msgs = (List.map (\m -> fetchControl m (.timer model)) (List.map (\(msg, _) -> msg) (List.concatMap (\s -> List.filter (\(_, desc) -> desc == s) controlDescMap) (List.concatMap (\g -> List.foldl (\(s,c) cs -> if Gamepad.wasClicked g c then s :: cs else cs) [] allMappableControls ) (Gamepad.Advanced.getGamepads allMappableControls (.userMappings model) blob)))))
        (m_, c_) = List.foldl (\msg (model_, cmds) -> (\(v,c) -> (v, cmds ++ [c])) <| update msg model_) ({ model | gamepadState = ActiveGamepad blob }, []) msgs
    in (m_, Cmd.batch c_)
--            processGamepadInput blob { model | gamepadState = ActiveGamepad blob }

-- OUTBOUND COMMUNICATION

broadcastIntent : Maybe WS.Socket -> Msg -> Cmd Msg
broadcastIntent ms msg = case msg of
    -- Splits controls
    StartSplit time  -> send ms <| startSplitsRequestJSON (Maybe.map T.posixToMillis time)
    Unsplit          -> send ms <| unsplitSplitsRequestJSON
    Skip             -> send ms <| skipSplitsRequestJSON
    Stop             -> send ms <| stopSplitsRequestJSON
    Reset timer      -> send ms <| resetSplitsRequestJSON timer
    -- Menu/navigation
    CloseSplits      -> send ms    closeSplitsRequestJSON
    ListGames        -> send ms    gameListRequestJSON
    ListCategories g -> case g of
        Nothing -> Cmd.none
        Just g_ -> send ms <| categoryListRequestJSON g_
    -- Load/editmanipulate splits
    LoadSplits c -> case c of
        Nothing -> Cmd.none
        Just c_ -> send ms <| splitsLoadRequestJSON c_
    LoadMultiCategory c -> send ms <| multiCategoryLoadRequest c
    -- Anything not explicitly matched can be safely ignored
    _                -> Cmd.none

splitsSave : Maybe WS.Socket -> Timer.Splits -> Cmd Msg
splitsSave ms s =
    case .runTracker s of
        -- TODO: This should be done as part of the editing phase.  Cest la vie
        Timer.SingleCategory r ->
            let game  = .game r
                cat   = .category r
                game_ = { game | name = .title s }
                cat_  = { cat | name = .subtitle s }
                run   = { r | game = game_, category = cat_ }
                gameJ = gameJSON game_
                catJ  = categoryJSON cat_
                segs  = segmentsJSON <| List.map .segment <| .upcoming <| .splits r
                json  = JE.object [ ( "tag", JE.string "NewSplits" )
                                  , ( "contents", JE.object [ ( "title", JE.string <| .title s )
                                                            , ( "subtitle", JE.string <| .subtitle s )
                                                            , ( "game", gameJ ), ( "category", catJ )
                                                            , ( "segments", segs )
                                                            ]
                                    )
                                  ]
            in send ms <| json
        Timer.MultiCategory rs ->
            let categoryIDs = JE.list (JE.int << (Maybe.withDefault 0) << .entityID << .category) <| .upcoming rs
                json = JE.object [ ( "tag", JE.string "NewMultiCategorySplits" )
                                 , ( "contents", JE.object [ ( "title", JE.string <| .title s )
                                                           , ( "categoryList", categoryIDs )
                                                           ]
                                   )
                                 ]
            in send ms json

gameJSON : Timer.Game -> JE.Value
gameJSON game = JE.object [ ( "name", JE.string <| .name game ), ( "icon", Maybe.withDefault JE.null <| Maybe.map JE.string <| .icon game ), ( "offset", JE.int <| .offset game ) ]

categoryJSON : Timer.Category -> JE.Value
categoryJSON cat = JE.object [ ( "name", JE.string <| .name cat ), ( "offset", JE.int <| .offset cat ) ]

segmentsJSON : List Timer.Segment -> JE.Value
segmentsJSON = JE.list (\seg -> JE.object [ ( "name", JE.string <| .name seg ), ( "icon", Maybe.withDefault JE.null <| Maybe.map JE.string <| .icon seg ) ])

newClientJSON : JE.Value
newClientJSON = JE.object [ ( "tag", JE.string "NewClient" )
                         , ( "contents", JE.null )
                         ]

closeSplitsRequestJSON : JE.Value
closeSplitsRequestJSON = JE.object [ ( "tag", JE.string "Menu" ), ( "contents", JE.object [ ( "tag", JE.string "MenuCloseSplits" ) ] ) ]

splitsLoadRequestJSON : Int -> JE.Value
splitsLoadRequestJSON category = JE.object [ ( "tag", JE.string "Menu" )
                                           , ( "contents", JE.object [ ( "tag", JE.string "MenuLoadSplits")
                                                                     , ( "contents", JE.int category )
                                                                     ]
                                             )
                                           ]

multiCategoryLoadRequest : Int -> JE.Value
multiCategoryLoadRequest category = JE.object [ ( "tag", JE.string "Menu" )
                                              , ( "contents", JE.object [ ( "tag", JE.string "MenuLoadMultiCategory" )
                                                                        , ( "contents", JE.int category )
                                                                        ]
                                                )
                                              ]

startSplitsRequestJSON : Maybe Int -> JE.Value
startSplitsRequestJSON time = case time of
    Just t -> JE.object [ ( "tag", JE.string "TimerControl" )
                        , ( "contents", JE.object [ ( "tag", JE.string "RemoteStartSplit" ), ( "contents", JE.int t ) ] )
                        ]
    _      -> JE.object [ ( "tag", JE.string "TimerControl" )
                        , ( "contents", JE.object [ ( "tag", JE.string "RemoteStartSplit" ), ( "contents", JE.null ) ] )
                        ]

skipSplitsRequestJSON : JE.Value
skipSplitsRequestJSON = JE.object [ ( "tag", JE.string "TimerControl" )
                                  , ( "contents", JE.object [ ( "tag", JE.string "RemoteSkip" ) ] )
                                  ]

unsplitSplitsRequestJSON : JE.Value
unsplitSplitsRequestJSON = JE.object [ ( "tag", JE.string "TimerControl" )
                                     , ( "contents", JE.object [ ( "tag", JE.string "RemoteUnsplit" ) ] )
                                     ]

stopSplitsRequestJSON : JE.Value
stopSplitsRequestJSON = JE.object [ ( "tag", JE.string "TimerControl" )
                                  , ( "contents", JE.object [ ( "tag", JE.string "RemoteStop" ) ] )
                                  ]

resetSplitsRequestJSON : Timer.Timer -> JE.Value
resetSplitsRequestJSON t = JE.object [ ( "tag", JE.string "TimerControl" )
                                     , ( "contents", JE.object [ ( "tag", JE.string "RemoteReset" )
                                                               , ( "contents", Timer.toJSON t )
                                                               ]
                                       )
                                     ]

gameListRequestJSON : JE.Value
gameListRequestJSON = JE.object [ ( "tag", JE.string "Menu" )
                                , ( "contents", JE.object [ ( "tag", JE.string "MenuGames" ) ] )
                                ]

categoryListRequestJSON : Int -> JE.Value
categoryListRequestJSON game = JE.object [ ( "tag", JE.string "Menu" )
                                         , ( "contents", JE.object [ ( "tag", JE.string "MenuCategories")
                                                                   , ( "contents", JE.int game )
                                                                   ]
                                           )
                                         ]

fullCategoryListRequestJSON : JE.Value
fullCategoryListRequestJSON = JE.object [ ( "tag", JE.string "Menu" )
                                        , ( "contents", JE.object [ ( "tag", JE.string "MenuFullCategories") ] )
                                        ]

finishSplitsRequestJSON : Timer.Timer -> JE.Value
finishSplitsRequestJSON t = JE.object [ ( "tag", JE.string "TimerControl" )
                                      , ( "contents", JE.object [ ( "tag", JE.string "RemoteFinish" )
                                                                , ( "contents", Timer.toJSON t )
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
        Ok (FetchedGameList games)    -> ( { model | gameList = .gameList games, multiCategoryList = .multiCategoryList games }, Cmd.none )
        Ok (FetchedCategoryList cats) -> ( { model | categoryList = cats }, Cmd.none )
        Ok (FullCategoryList runs)    -> ( { model | fullCategoryList = runs }, Cmd.none )
        Ok (FetchedSplits s)          -> ( { model | timer = Timer.load s }, Cmd.none )
        Ok (ConfigStoreSet k v)       -> ( { model | configStore = Dict.insert k v (.configStore model) }, broadcastIntent (.socket model) (LoadSplits (String.toInt v) ) )
        _                             -> ( model, Cmd.none )

processSplitsControl : Model -> SplitsMessage -> (Model, Cmd Msg)
processSplitsControl model msg = case msg of
    RemoteStartSplit t -> timerControl False model (StartSplit <| Maybe.map (\time -> T.millisToPosix <| (T.posixToMillis time) + t) (.runStarted <| Timer.splitsFor <| .timer model))
    RemoteUnsplit      -> timerControl False model Unsplit
    RemoteSkip         -> timerControl False model Skip
    RemoteStop         -> timerControl False model Stop
    RemoteReset        -> timerControl False model (Reset <| .timer model)

processIncomingEvent : JE.Value -> Result JD.Error WebsocketMessage
processIncomingEvent =
    JD.decodeValue <| JD.oneOf [ clientStateRequestDecoder
                               , splitsControlDecoder
                               , gameListDecoder
                               , categoryListDecoder
                               , fullCategoryListDecoder
                               , splitsSpecDecoder
                               , multiCategoryDecoder
                               , closeSplitsDecoder
                               , splitsControlDecoder
                               , configStoreDecoder
                               ]

closeSplitsDecoder : JD.Decoder WebsocketMessage
closeSplitsDecoder = checkTag "CloseSplits" <| JD.map (\_ -> UnloadSplits) ( JD.at [ "data", "tag" ] JD.string )

gameListDecoder : JD.Decoder WebsocketMessage
gameListDecoder =
    checkTag "GameList" <| JD.map FetchedGameList <| JD.map2 FullGameList
        ( JD.at [ "data", "contents", "gamesList" ] <| JD.list
            ( JD.map4 Timer.Game ( JD.at [ "gameID" ] <| JD.maybe JD.int )
                                 ( JD.at [ "gameData", "gameName" ] JD.string )
                                 ( JD.at [ "gameData", "gameIcon" ] <| JD.maybe JD.string )
                                 ( JD.at [ "gameData", "gameDefaultOffset" ] JD.int )
            )
        )
        ( JD.at [ "data", "contents", "multiCategoriesList" ] <| JD.list
            ( JD.map2 MultiCategoryInfo ( JD.at [ "multiCategoryName" ] JD.string )
                                        ( JD.at [ "multiCategoryID" ] JD.int )
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

fullCategoryListDecoder : JD.Decoder WebsocketMessage
fullCategoryListDecoder =
    checkTag "FullCategoryList" <| JD.map FullCategoryList
        ( JD.at [ "data", "contents" ] <| JD.list runSpecDecoder )

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

multiCategoryDecoder : JD.Decoder WebsocketMessage
multiCategoryDecoder =
    checkTag "MultiCategoryRefresh" <| JD.map FetchedSplits <|
        JD.map2 (\t cs -> Timer.SplitsSpec t "" cs) ( JD.at [ "data", "contents", "multiCategoryTitle" ] JD.string )
                                                    ( JD.at [ "data", "contents" ] runSpecDecoder )

runSpecDecoder : JD.Decoder Timer.RunSpec
runSpecDecoder = JD.oneOf [ singleCategorySpecDecoder, multiCategorySpecDecoder ]

singleCategorySpecDecoder : JD.Decoder Timer.RunSpec
singleCategorySpecDecoder = JD.map Timer.SingleCategorySpec singleCategorySpecDecoder_

singleCategorySpecDecoder_ : JD.Decoder Timer.Run
singleCategorySpecDecoder_ =
    JD.map3 (Timer.Run Nothing Nothing) gameDecoder
                                        categoryDecoder
                                        ( JD.at [ "splitSetSegments" ] splitSetDecoder )

multiCategorySpecDecoder : JD.Decoder Timer.RunSpec
multiCategorySpecDecoder =
    JD.map2 Timer.MultiCategorySpec (JD.at [ "multiCategoryID" ] <| JD.int)
                                    (JD.at [ "multiCategoryGames" ] <| JD.list singleCategorySpecDecoder_)

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
    case .gamepadState model of
        RemappingGamepad _ ->
            Sub.batch [ WS.subscriptions SocketOpened SocketNotOpened SocketReceived
                      , Sub.map KeyboardEvent KB.subscriptions
                      , T.every 4 Tick
                      , GamepadPort.onBlob (GamepadRemappingTool << Gamepad.Advanced.onBlob)
                      , GamepadPort.onLoad GamepadMappingLoad
                      ]
        _ ->
            Sub.batch [ WS.subscriptions SocketOpened SocketNotOpened SocketReceived
                      , Sub.map KeyboardEvent KB.subscriptions
                      , T.every 4 Tick
                      , GamepadPort.onBlob GamepadFrame
                      , GamepadPort.onLoad GamepadMappingLoad
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
                                          , Html.span [ HA.classList [("time-separator", True), ("time-minutes-separator", True), ("time-empty", minutesEmpty)] ] [ Html.text ":" ]
                                          , Html.span [ HA.classList [("time-seconds", True)] ] [ Html.text <| String.padLeft 2 '0' <| String.fromInt <| .seconds d ]
                                          , Html.span [ HA.classList [("time-separator", True), ("time-seconds-separator", True)] ] [ Html.text "." ]
                                          , Html.span [ HA.classList [("time-millis", True)] ] [ Html.text <| String.padLeft 2 '0' <| String.fromInt <| (.millis d) // 10 ]
                                          ]

categoryTimer : Timer.Timer -> Html Msg
categoryTimer timer =
    let splits = Timer.splitsFor <| timer
        currentTime_ = .currentTime splits
        currentTime = T.posixToMillis currentTime_
        runStarted  = T.posixToMillis <| Maybe.withDefault currentTime_ <| Maybe.withDefault Nothing <| Maybe.map .runStarted <| Timer.runFor timer
        diffTime = case timer of
                       Timer.Finished _ -> case Timer.runFor timer of
                                               Nothing -> -1 * (currentTime - runStarted) -- Required to make the function total; should be unreachable
                                               Just r  -> (T.posixToMillis <| Maybe.withDefault currentTime_ <| .runEnded r) - runStarted
                       _ -> currentTime - runStarted
    in Html.div [ HA.id "main-timer", HA.classList <| timeStatus timer ] [ showD False [] (unitize diffTime) ]

allCategoriesTimer : Timer.Timer -> Html Msg
allCategoriesTimer timer =
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
                                   Nothing -> [("ahead", True), ("gaining", True)] -- Impossible (except during multi-category runs in between categories or first runs of a given category) (TODO)
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
                 , categoryTimer timer
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
                                  Just r  -> [r]
                                  Nothing -> case .upcoming rs of
                                          (r :: _) -> [r]
                                          [] -> List.drop ((List.length (.previous rs)) - 1) (.previous rs)
                in Html.div [ HA.id "splits-container" ] <| splitHeaders -- TODO: This needs a specialized view
                    ++ (List.concatMap (splitsList_ max) <| List.map (Timer.aggregateRun currentTime) current)

splitsList_ : Int -> Timer.TimingData -> List (Html Msg)
splitsList_ max timing =
    let current_ = case (.current timing) of
                      Nothing -> []
                      Just t  -> [t]
        f s = List.map (\x -> (s, x))
        last = f "upcoming" <| List.take 1 <| List.reverse (.upcoming timing)
        pre1 = f "previous" <| List.take 1 (List.reverse (.previous timing))
        current = f "current" <| current_
        upcoming = f "upcoming" <| List.take (max - (List.length (pre1 ++ last ++ current))) (List.reverse <| List.drop 1 <| List.reverse (.upcoming timing))
        pre = f "previous" <| List.reverse <| List.take (max - (List.length (pre1 ++ current ++ upcoming ++ last))) (List.drop 1 (List.reverse (.previous timing)))
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

timerViewEdit : List Timer.RunSpec -> Bool -> Timer.Timer -> Html Msg
timerViewEdit categories multicategory timer =
    let splits = Timer.splitsFor timer
        header = if multicategory then [ Html.div [ HA.id "timer-multicategory" ] [ Html.input [ HA.type_ "checkbox", HA.checked multicategory, HE.onClick <| UpdateMultiCategory False ] [] ]
                                       , Html.div [ HA.id "timer-title" ] [ Html.input [ HA.type_ "text", HA.placeholder "Title", HA.value <| .title splits, HE.onInput UpdateTitle ] [] ]
                                       ]
                                  else [ Html.div [ HA.id "timer-multicategory" ] [ Html.input [ HA.type_ "checkbox", HA.checked multicategory, HE.onClick <| UpdateMultiCategory True ] [] ]
                                       , Html.div [ HA.id "timer-title" ] [ Html.input [ HA.type_ "text", HA.placeholder "Title", HA.value <| .title splits, HE.onInput UpdateTitle ] [] ]
                                       , Html.div [ HA.id "timer-subtitle" ] [ Html.input [ HA.type_ "text", HA.placeholder "Category", HA.value <| .subtitle splits, HE.onInput UpdateSubtitle ] [] ]
                                       ]
        footer = if multicategory then [ Html.div [ HA.id "timer-edit-add" ] [ Html.span [ HE.onClick AddGame ] [ Html.text "Add Category" ] ]
                                       , Html.div [ HA.id "timer-edit-options" ] [ Html.span [ HE.onClick EditSplitsCancel] [ Html.text "[ Cancel ]" ]
                                                                                 , Html.span [ HE.onClick EditSplitsSave ] [ Html.text "[ Save ]" ]
                                                                                 ]
                                       ]
                                  else [ Html.div [ HA.id "timer-edit-add" ] [ Html.span [ HE.onClick AddSegment ] [ Html.text "Add Segment" ] ]
                                       , Html.div [ HA.id "timer-edit-options" ] [ Html.span [ HE.onClick EditSplitsCancel] [ Html.text "[ Cancel ]" ]
                                                                                 , Html.span [ HE.onClick EditSplitsSave ] [ Html.text "[ Save ]" ]
                                                                                 ]
                                       ]
    in Html.div [ HA.id "timer-container" ] <| header ++ [splitsEdit categories splits] ++ footer

splitsEdit : List Timer.RunSpec -> Timer.Splits -> Html Msg
splitsEdit categories splits =
    case .runTracker splits of
        Timer.SingleCategory r -> Html.div [] <| List.foldl splitsEdit_ [] <| List.indexedMap Tuple.pair <| (.upcoming << .splits) r
        Timer.MultiCategory rs -> Html.div [] <| List.foldl (categoriesEdit categories) [] <| List.indexedMap Tuple.pair (.upcoming rs)

splitsEdit_ : (Int, Timer.Split) -> List (Html Msg) -> List (Html Msg)
splitsEdit_ (i, s) hs = let name = .name <| .segment s in
    hs ++ [ Html.div [] [ Html.div [ HA.class "split-name" ] [ Html.input [ HA.type_ "text"
                                                                          , HA.placeholder <| "Segment #" ++ (String.fromInt <| 1 + i)
                                                                          , HA.value name
                                                                          , HE.onInput (UpdateSegmentName i)
                                                                          ]
                                                                          [ ]
                                                             ]
                        ]
         ]

categoriesEdit : List Timer.RunSpec -> (Int, Timer.Run) -> List (Html Msg) -> List (Html Msg)
categoriesEdit categories (i, r) hs =
    let name = (.name <| .game r) ++ " - " ++ (.name <| .category r)
    in hs ++ [ Html.div [] [ Html.div [ HA.class "split-name" ] [ Html.select [ HE.onInput (UpdateSelectedGame i) ]
                                                                              ( List.map (categoriesOptions (.entityID <| .category r)) categories )
                                                                ]
                           ]
             ]

categoriesOptions : Maybe Int -> Timer.RunSpec -> Html Msg
categoriesOptions active rs =
    case rs of
        Timer.MultiCategorySpec _ _ -> Html.option [] [ Html.text "Error: Multi-category run" ]
        Timer.SingleCategorySpec r ->
            let cid = case (.entityID <| .category r) of
                          Just i -> [ HA.value <| String.fromInt i, HA.selected (active == (.entityID <| .category r)) ]
                          Nothing -> []
            in Html.option cid [ Html.text <| (.name <| .game r) ++ " - " ++ (.name <| .category r) ]

timerView : Model -> Html Msg
timerView model = case .splitsMode model of
    NormalSplitsView -> timerViewNormal (.maxSegmentsShown model) (.timer model)
    EditSplitsView -> timerViewEdit (.fullCategoryList model) (.editMulticategory model) (.timer model)

menuView : Model -> List (Html Msg)
menuView model = case .menu model of
    MenuHidden -> []
    MenuVisible nav -> [ Html.div [ HA.class "modal-background" ] []
                       , Html.div [ HA.class "modal" ] (menuView_ model nav)
                       ]

{- Adapted from Gamepad example code TODO -}
viewDigital : Gamepad -> ( String, Gamepad.Digital ) -> Html msg
viewDigital gamepad ( name, digital ) =
    let s = case (Gamepad.isPressed gamepad digital) of
                True -> "True"
                False -> "False"
    in Html.li [] [ Html.text <| s ++ " <- " ++ name ]

viewGamepad : Gamepad -> Html Msg
viewGamepad gamepad =
    Html.div
        []
        [ Html.h3 [] [ "Gamepad " ++ String.fromInt (Gamepad.getIndex gamepad) |> Html.text ]
        , Html.div [] (allMappableControls |> List.map (viewDigital gamepad))
        ]

viewGamepads : Model -> Blob -> Html Msg
viewGamepads model blob =
    let views = List.map viewGamepad <| Gamepad.Advanced.getGamepads allMappableControls (.userMappings model) blob
    in
    if List.length views > 0 then
        Html.div [] views
    else
        Html.div [ ] [ Html.div [] [ Html.text "Can't find any gamepad! =(" ]
                     , Html.div [] [ Html.text "(The browser won't tell me they are there unless you press some button first, so maybe try that)" ]
                     ]
{- End adapted from Gamepad example code TODO -}

menuView_ : Model -> MenuNav -> List (Html Msg)
menuView_ model nav = case nav of
    MainMenu -> [ Html.div [ HA.class "menu-button", HE.onClick ListGames ] [ Html.text "Load Splits" ]
                , Html.div [ HA.class "menu-button", HE.onClick EditSplits ] [ Html.text "Edit Splits" ]
                , Html.div [ HA.class "menu-button", HE.onClick CloseSplits ] [ Html.text "Close Splits" ]
                , Html.div [ HA.class "menu-button", HE.onClick InputConfig ] [ Html.text "Edit Bindings" ]
                ]
    GameList ->
        (List.map (\g -> Html.div [ HA.class "menu-button", HE.onClick (ListCategories <| .entityID g)] [ Html.text <| .name g ]) (.gameList model))
        ++ [ Html.div [] [ Html.text "---" ] ]
        ++ (List.map (\c -> Html.div [ HA.class "menu-button", HE.onClick (LoadMultiCategory <| .entityID c)] [ Html.text <| .name c ]) (.multiCategoryList model))
    CategoryList -> List.map (\c -> Html.div [ HA.class "menu-button", HE.onClick (LoadSplits <| .entityID c)] [ Html.text <| .name c ]) (.categoryList model)
    Config -> case .gamepadState model of
        Uninitialized -> [ Html.div [ ] [ Html.text "Awaiting Gamepad input" ] ]
        ActiveGamepad b -> [ Html.div [ HA.class "menu-button", HE.onClick RemapGamepadToggle ] [ Html.text "Remap Input" ]
                           , Html.div [] []
                           , Html.div [ ] [ viewGamepads model b ]
                           ]
        RemappingGamepad m -> [ Html.div [ HA.class "menu-button", HE.onClick RemapGamepadToggle ] [ Html.text "Close Remap Tool" ]
                              , Html.div [] []
                              , Html.div [] [ Html.map GamepadRemappingTool <| Gamepad.Advanced.view (.userMappings model) m ]
                              ]

view : Model -> Html Msg
view model =
    Html.div [ HA.id "main-container" ]
             [ Html.div [ HA.id "app-top" ]
                        ( (menuView model) ++
                          [ timerView model
                          ]
                        )
             ]
