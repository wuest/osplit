module Types exposing (..)

import Json.Encode as JE
import Time        as T

type alias SocketSpec = (String, List String)

type alias Socket = { url : String
                    , fd : Int
                    }

type Msg = OpenSocket String
         | SendSocket JE.Value
         | SocketOpened Socket
         | SocketNotOpened
         | SocketReceived JE.Value
         -- Top-level app navigation
         | SplitsMenu
         | ListSplits
         | CloseSplits
         -- Timer Controls
         | StartSplitFinish Int
         | Stop Int
         | Reset Int
         | Unsplit Int
         -- Timer backend
         | Tick T.Posix
         | SyncTime T.Posix

type WebsocketMessage = TimeSync TimeSyncResponse
                      | SplitsControl SplitsMessage

type alias TimeSyncResponse = { currentTime : Int
                              , previousOffset : Int
                              }

type SplitsMessage = RemoteStartSplit Int
                   | RemoteUnsplit Int
                   | RemoteStop Int
                   | RemoteReset Int
