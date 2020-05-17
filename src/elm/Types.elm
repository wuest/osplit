module Types exposing (..)

import Timer.Types exposing ( Timer, Game, Category, SplitsSpec )

import Json.Encode as JE
import Time        as T
import Dict        as Dict

type alias SocketSpec = (String, List String)

type alias Socket = { url : String
                    , fd : Int
                    }

type alias CSKey       = String
type alias CSVal       = String
type alias ConfigStore = Dict.Dict CSKey CSVal

type Msg = OpenSocket String
         | SendSocket JE.Value
         | SocketOpened Socket
         | SocketNotOpened
         | SocketReceived JE.Value
         -- Top-level app navigation
         | ToggleMainMenu
         | ListGames
         | ListCategories (Maybe Int)
         | LoadSplits (Maybe Int)
         | CloseSplits
         -- Timer Controls
         | StartSplit Int
         | Finish Timer
         | Unsplit Int
         | Skip Int
         | Stop Int
         | Reset Timer
         -- Timer backend
         | Tick T.Posix
         | SyncTime T.Posix

type WebsocketMessage = TimeSync TimeSyncResponse
                      | ClientStateRequest Int
                      | SplitsControl SplitsMessage
                      | UnloadSplits
                      | FetchedGameList (List Game)
                      | FetchedCategoryList (List Category)
                      | FetchedSplits (Maybe SplitsSpec)
                      | ConfigStoreSet CSKey CSVal

type alias TimeSyncResponse = { currentTime : Int
                              , previousOffset : Int
                              }

type SplitsMessage = RemoteStartSplit Int
                   | RemoteUnsplit Int
                   | RemoteSkip Int
                   | RemoteStop Int
                   | RemoteReset
