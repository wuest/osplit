port module GamepadPort exposing (..)

import Gamepad.Advanced exposing (UserMappings, Blob, userMappingsToString, userMappingsFromString)


port onBlob : (Blob -> msg) -> Sub msg
port onLoad : (String -> msg) -> Sub msg
port saveMappings : String -> Cmd msg
port loadMappings : String -> Cmd msg

save : UserMappings -> Cmd msg
save = saveMappings << userMappingsToString

load : Cmd msg
load = loadMappings ""

fromString : String -> UserMappings -> UserMappings
fromString s orig =
    case userMappingsFromString s of
        Ok mappings -> mappings
        _ -> orig
