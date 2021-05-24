port module Websocket exposing ( Socket
                               , open, openWithProto, openWithProtos
                               , createWS, send
                               , subscriptions
                               )

import Json.Encode as JE
import Json.Decode as JD

type alias SocketSpec = (String, List String)

type alias Socket = { url : String
                    , fd : Int
                    }

type alias SocketData = { socket : Socket
                        , data : JE.Value
                        }

port createWS : SocketSpec -> Cmd msg
port sendWS : SocketData -> Cmd msg

port newFD : (JD.Value -> msg) -> Sub msg
port recv : (JE.Value -> msg) -> Sub msg

open : String -> Cmd msg
open url = createWS (url, [])

openWithProto : String -> String -> Cmd msg
openWithProto url proto = createWS (url, [proto])

openWithProtos : String -> List String -> Cmd msg
openWithProtos url protos = createWS (url, protos)

send : Socket -> JE.Value -> Cmd msg
send s v = sendWS { socket = s, data = v }

decodeSocket : JD.Value -> Result JD.Error Socket
decodeSocket = JD.decodeValue <| JD.map2 Socket
    (JD.field "url" JD.string)
    (JD.field "fd" JD.int)

processNewFD : (Socket -> msg) -> (JD.Error -> msg) -> JD.Value -> msg
processNewFD msgOpened msgNotOpened value = case (decodeSocket value) of
    Ok socket -> msgOpened socket
    Err msg   -> msgNotOpened msg

subscriptions : (Socket -> msg) -> (JD.Error -> msg) -> (JE.Value -> msg) -> Sub msg
subscriptions opened notOpened received =
    Sub.batch [ newFD <| processNewFD opened notOpened
              , recv received
              ]
