module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Encode as JE
import Json.Decode as JD

import Types exposing (Msg(..), Socket, WebsocketMessage(..), SplitsMessage(..), TimeSyncResponse)
import Websocket as WS
import Timer as Timer

import Debug as D

main = Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

{- MODEL -}

type alias Model = { socket : Maybe Socket
                   , timer : Timer.Timer
                   , timeOffset : Int
                   , activePane : TimerPane
                   }

type TimerPane = Splits | Menu

init : String -> (Model, Cmd Msg)
init url = ( { socket = Nothing
             , timer = Timer.empty
             , timeOffset = 0
             , activePane = Splits
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
            ({ model | socket = Just newsocket }, WS.send newsocket <| Timer.syncRequest (.timer model))
        SocketNotOpened ->
            (model, Cmd.none)
        SocketReceived data ->
            (processData model data, Cmd.none)
        SyncTime _ ->
            (model, send (.socket model) (Timer.syncRequest (.timer model)))
        ListSplits ->
            (model, Cmd.none) -- SPOT
        _ ->
            ({ model | timer = Timer.update msg <| .timer model }, broadcastIntent (.socket model) msg)

broadcastIntent : Maybe Socket -> Msg -> Cmd Msg
broadcastIntent ms msg =
    case msg of
        StartSplitFinish t -> send ms <| startSplitAnnounce t
        Unsplit t          -> send ms <| unsplitAnnounce t
        Stop t             -> send ms <| stopAnnounce t
        Reset t            -> send ms <| resetAnnounce t
        _                  -> Cmd.none

startSplitAnnounce : Int -> JE.Value
startSplitAnnounce t = JE.object [ ( "tag", JE.string "TimerControl" )
                                 , ( "contents", JE.object [ ( "tag", JE.string "RemoteStartSplit" )
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

resetAnnounce : Int -> JE.Value
resetAnnounce t = JE.object [ ( "tag", JE.string "TimerControl" )
                            , ( "contents", JE.object [ ( "tag", JE.string "RemoteReset" )
                                                      , ( "contents", JE.int t )
                                                      ]
                              )
                            ]

processData : Model -> JE.Value -> Model
processData model data =
    case processIncomingEvent data of
        Ok (TimeSync response) -> processTimeSync model response
        Ok (SplitsControl ctl) -> { model | timer = Timer.update (remoteControl ctl) (.timer model) }
        x                      -> model

processIncomingEvent : JE.Value -> Result JD.Error WebsocketMessage
processIncomingEvent = 
        JD.decodeValue <| JD.oneOf [ timeSyncResponseDecoder, splitsControlDecoder ]

timeSyncResponseDecoder : JD.Decoder WebsocketMessage
timeSyncResponseDecoder =
    JD.map TimeSync <| 
        JD.map2 TimeSyncResponse ( JD.at [ "data", "contents", "currentTime" ] JD.int )
                                 ( JD.at [ "data", "contents", "previousOffset" ] JD.int )

splitsControlDecoder : JD.Decoder WebsocketMessage
splitsControlDecoder = JD.oneOf [ decodeRemoteStartSplit, decodeRemoteUnsplit, decodeRemoteStop, decodeRemoteReset ]

decodeRemoteStartSplit : JD.Decoder WebsocketMessage
decodeRemoteStartSplit = checkTag2 "RemoteStartSplit" (JD.map SplitsControl (JD.map RemoteStartSplit <| JD.at [ "data", "contents", "contents" ] JD.int))

decodeRemoteUnsplit : JD.Decoder WebsocketMessage
decodeRemoteUnsplit = checkTag2 "RemoteUnsplit" (JD.map SplitsControl (JD.map RemoteUnsplit <| JD.at [ "data", "contents", "contents" ] JD.int))

decodeRemoteStop : JD.Decoder WebsocketMessage
decodeRemoteStop = checkTag2 "RemoteStop" (JD.map SplitsControl (JD.map RemoteStop <| JD.at [ "data", "contents", "contents" ] JD.int))

decodeRemoteReset : JD.Decoder WebsocketMessage
decodeRemoteReset = checkTag2 "RemoteReset" (JD.map SplitsControl (JD.map RemoteReset <| JD.at [ "data", "contents", "contents" ] JD.int))

checkTag : String -> JD.Decoder a -> JD.Decoder a
checkTag target da = checkTag_ (JD.at [ "data", "tag" ] JD.string) target da

checkTag2 : String -> JD.Decoder a -> JD.Decoder a
checkTag2 target da = checkTag_ (JD.at [ "data", "contents", "tag" ] JD.string) target da

checkTag_ : JD.Decoder String -> String -> JD.Decoder a -> JD.Decoder a
checkTag_ ds target da =
    ds |> JD.andThen (\s -> if s == target then da else JD.fail ("No match on " ++ s))

processTimeSync : Model -> TimeSyncResponse -> Model
processTimeSync model response = { model | timeOffset = Timer.processSyncResponse (.timer model) response }

remoteControl : SplitsMessage -> Msg
remoteControl s =
    case s of
        RemoteStartSplit t -> StartSplitFinish t
        RemoteUnsplit t    -> Unsplit t
        RemoteStop t       -> Stop t
        RemoteReset t      -> Reset t

{- SUBSCRIPTIONS -}
subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch [ WS.subscriptions
                            , Timer.subscriptions
                            ]

{- VIEW -}
li : String -> Html Msg
li string = Html.li [] [Html.text string]

timerView model = 
    case .activePane model of
        Splits -> Html.div [ HA.id "timer-top" ]
                           [ Timer.view <| .timer model ]
        Menu -> Html.div [ HA.id "timer-top" ]
                         [ Timer.edit <| .timer model ]

menuContainer : TimerPane -> Html Msg
menuContainer state = case state of
    Splits -> Html.div [ HA.id "menu-container" ]
                       [ Html.div [ HA.class "menu-button"
                                  , HE.onClick SplitsMenu
                                  ]
                                  [ Html.text "☰" ]
                       ]
    Menu   -> Html.div [ HA.id "menu-container" ]
                       [ Html.div [ HA.class "menu-button"
                                  , HE.onClick SplitsMenu
                                  ]
                                  [ Html.text "☰" ]
                       , Html.div [ HA.class "menu-button"
                                  , HE.onClick CloseSplits
                                  ]
                                  [ Html.text "New" ]
                       , Html.div [ HA.class "menu-button"
                                  , HE.onClick ListSplits
                                  ]
                                  [ Html.text "Load" ]
                       ]


view : Model -> Html Msg
view model =
    Html.div [ HA.id "main-container" ]
             [ Html.div [ HA.id "app-top" ]
                        [ timerView model
                        ]
             , menuContainer <| .activePane model
             ]
