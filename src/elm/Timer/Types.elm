module Timer.Types exposing ( .. )

import Time            as T

type Timer = Stopped Splits
           | Running Splits

type Position = Previous
              | Current
              | Upcoming

type TimeStatus = Ahead
                | Behind
                | Gold

type TimeChange = Gaining TimeStatus
                | Losing TimeStatus
                | Skipped

type alias Time = T.Posix
type alias Icon = Maybe String
type alias SplitSet = List Split

type alias Segment = { entityID : Maybe Int
                     , name     : String
                     , icon     : Icon
                     , pb       : Maybe Int
                     , gold     : Maybe Int
                     , average  : Maybe Int
                     , worst    : Maybe Int
                     }

type alias Split = { segment : Segment
                   , time    : Maybe Int
                   , change  : Maybe TimeChange
                   }

type alias Game = { entityID : Maybe Int
                  , name     : String
                  , icon     : Icon
                  , offset   : Int
                  }

type alias Splits = { started   : Time
                    , current   : Time
                    , elapsed   : Int
                    , game      : Game
                    , category  : String
                    , passed    : SplitSet
                    , split     : Maybe Split
                    , remaining : SplitSet
                    }

type alias TimeSum = { pb : Int
                     , gold : Int
                     , average : Int
                     , worst : Int
                     , current : Int
                     }
