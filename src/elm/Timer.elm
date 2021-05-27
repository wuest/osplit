module Timer exposing ( Timer(..), RunTracker(..), TimingData, TimingDatum
                      , mapT, mapR
                      , splitsFor, runFor, aggregateRun
                      , Game, Category, Run, Splits, SplitSet, Split, Segment, RunSpec(..), SplitsSpec
                      , empty, emptySegment, emptySplitSet
                      , start, stop, reset, split, unsplit, skip, setTime
                      , load, toJSON
                      , gameDecoder, categoryDecoder, segmentDecoder, segmentListDecoder, runSpecDecoder
                      )

import Time        as T
import Json.Encode as JE
import Json.Decode as JD

type alias Time = T.Posix
type alias Icon = Maybe String

type Timer = Stopped  Splits
           | Paused   Splits
           | Finished Splits
           | Running  Splits

type alias Game = { entityID : Maybe Int
                  , name     : String
                  , icon     : Icon
                  , offset   : Int
                  }

type alias Category = { entityID : Maybe Int
                      , name     : String
                      , offset   : Int
                      }

type alias Segment = { entityID : Maybe Int
                     , name     : String
                     , icon     : Icon
                     , pb       : Maybe Int
                     , gold     : Maybe Int
                     , average  : Maybe Int
                     , worst    : Maybe Int
                     }

type SplitStatus = Incomplete
                 | Skipped
                 | Completed Time

type alias Split = { segment     : Segment
                   , endTime     : Maybe Time
                   , segmentTime : Maybe Time
                   }

type alias SplitSet = { previous : List Split
                      , current  : Maybe Split
                      , upcoming : List Split
                      }

type alias Run = { runStarted : Maybe Time
                 , runEnded   : Maybe Time
                 , game       : Game
                 , category   : Category
                 , splits     : SplitSet
                 }

type alias RunSet = { previous : List Run
                    , current  : Maybe Run
                    , upcoming : List Run
                    }

type RunTracker = SingleCategory Run
                | MultiCategory RunSet

type alias Splits = { runStarted  : Maybe Time
                    , runEnded    : Maybe Time
                    , currentTime : Time
                    , title       : String
                    , subtitle    : String
                    , runTracker  : RunTracker
                    }

type RunSpec = SingleCategorySpec Run
             | MultiCategorySpec (List Run)

type alias SplitsSpec = { title    : String
                        , subtitle : String
                        , run      : RunSpec
                        }

type alias TimingDatum = { segment    : Segment
                         , running    : Segment
                         , singleTime : Maybe Int
                         , currentSum : Maybe Int
                         , timingTags : List (String, Bool)
                         }

type alias TimingData = { previous : List TimingDatum
                        , current : Maybe TimingDatum
                        , upcoming : List TimingDatum
                        }

{- HELPER FUNCTIONS -}
epoch : Time
epoch = T.millisToPosix 0

noGame : Game
noGame = { entityID = Nothing
         , name     = "(No Game)"
         , icon     = Nothing
         , offset   = 0
         }

noCategory : Category
noCategory = { entityID = Nothing
             , name     = "(No Category)"
             , offset   = 0
             }

emptySegment : Segment
emptySegment = { entityID = Nothing
               , name     = "---"
               , icon     = Nothing
               , pb       = Nothing
               , gold     = Nothing
               , average  = Nothing
               , worst    = Nothing
               }

initSegment : Segment
initSegment = { entityID = Nothing
              , name     = "Run aggregation initial segment"
              , icon     = Nothing
              , pb       = Nothing
              , gold     = Nothing
              , average  = Nothing
              , worst    = Nothing
              }

emptySplitSet : SplitSet
emptySplitSet = { previous = []
                , current  = Nothing
                , upcoming = [ { segment = emptySegment
                               , endTime = Nothing
                               , segmentTime = Nothing
                               }
                             ]
                }

noRun : Run
noRun = { runStarted = Nothing
        , runEnded   = Nothing
        , game       = noGame
        , category   = noCategory
        , splits     = emptySplitSet
        }

noSingleRun : RunTracker
noSingleRun = SingleCategory noRun

tfm : Maybe Time -> Time
tfm = Maybe.withDefault epoch

mapT : (Splits -> Splits) -> Timer -> Timer
mapT f t = case t of
    Stopped  s -> Stopped  <| f s
    Paused   s -> Paused   <| f s
    Finished s -> Finished <| f s
    Running  s -> Running  <| f s

mapR : (Run -> Run) -> Splits -> Splits
mapR f splits = case (.runTracker splits) of
    SingleCategory r -> { splits | runTracker = SingleCategory <| f r }
    MultiCategory rs ->
        let c = .current rs
        in case c of
            Nothing -> splits
            Just r  -> { splits | runTracker = MultiCategory { rs | current = Just <| f r } }

aggregateRun : Int -> Run -> TimingData
aggregateRun now r =
    let previous = .previous <| .splits r
        upcoming = .upcoming <| .splits r
        current = case .current (.splits r) of
                      Nothing -> []
                      Just s  -> [s]
        zeroSegment = { initSegment | pb = Just 0, gold = Just 0, average = Just 0, worst = Just 0 }
        ((sums1, pastSegment, _), runningTotalsP) = List.foldl (aggregateRun_ now) ((zeroSegment, initSegment, Just 0), []) previous
        (currentSegment, runningTotalC_) = List.foldl (aggregateRun_ now) ((sums1, pastSegment, Nothing), []) current
        (_, runningTotalsU) = List.foldl (aggregateRun_ now) (currentSegment, []) upcoming
        runningTotalC = case runningTotalC_ of
                            [] -> Nothing
                            x :: _ -> Just x
    in { previous = runningTotalsP, current = runningTotalC, upcoming = runningTotalsU }

-- PB is relative timestamp from the run; all other values are by-segment
aggregateRun_ : Int -> Split -> ((Segment, Segment, Maybe Int), List TimingDatum) -> ((Segment, Segment, Maybe Int), List TimingDatum)
aggregateRun_ now currentSplit ((sums, last, lastTime), segs) =
    let seg = .segment currentSplit
        currentTime = Maybe.map (\t -> (T.posixToMillis t) - now) <| .endTime currentSplit
        indiv = { seg | pb       = case (Maybe.map (\t -> t - (Maybe.withDefault 0 <| .pb last)) (.pb seg)) of
                                 Nothing -> .pb last
                                 x       -> x
                      , gold     = .gold seg
                      , average  = .average seg
                      , worst    = .worst seg
                }

        nextSegment = { seg | pb      = case .pb seg of
                                            Nothing -> .pb last
                                            x       -> x
                            , gold    = case .gold seg of
                                            Nothing -> .gold last
                                            x       -> x
                            , average = case .average seg of
                                            Nothing -> .average last
                                            x       -> x
                            , worst   = case .worst seg of
                                            Nothing -> .worst last
                                            x       -> x
                            }
        sums_ = { sums | pb = .pb seg
                       , gold = Maybe.map ((+) (Maybe.withDefault 0 <| .gold seg)) <| .gold sums
                       , average = Maybe.map ((+) (Maybe.withDefault 0 <| .average seg)) <| .average sums
                       , worst = Maybe.map ((+) (Maybe.withDefault 0 <| .worst seg)) <| .worst sums
                }
        singleTime = Maybe.map2 (-) currentTime lastTime
        nextTime = case currentTime of
                       Nothing -> lastTime
                       x -> x
        tags = statusTags indiv seg singleTime currentTime
    in ((sums_, nextSegment, nextTime), segs ++ [{segment = indiv, running = sums_, singleTime = singleTime, currentSum = currentTime, timingTags = tags}])

statusTags : Segment -> Segment -> Maybe Int -> Maybe Int -> List (String, Bool)
statusTags indiv running singleTime currentTime =
    case singleTime of
        Nothing -> []
        Just st -> case .gold indiv of
            Nothing ->
                [ ("gaining", True), ("ahead", True) ] -- No gold but active run means fresh split
            Just g ->
                if g > st
                then [ ("gold", True) ]
                else case .pb indiv of
                    Just pb ->
                        let gaining = st <= pb
                            losing = pb < st
                        in case .pb running of
                            Nothing -> [ ("gaining", gaining), ("losing", losing), ("ahead", True) ]
                            Just r  -> case currentTime of
                                Nothing -> [ ("gaining", gaining), ("losing", losing), ("ahead", True) ]
                                Just ct -> [ ("gaining", gaining), ("losing", losing), ("ahead", ct <= r), ("behind", ct > r)]
                    Nothing -> []

{- TIMER FUNCTIONS -}
empty : Timer
empty = Stopped empty_

empty_ : Splits
empty_ = { runStarted   = Nothing
         , runEnded     = Nothing
         , currentTime  = epoch
         , title        = ""
         , subtitle     = ""
         , runTracker   = noSingleRun
         }

load : SplitsSpec -> Timer
load newSplits = Stopped { empty_ | title = .title newSplits
                                  , subtitle = .subtitle newSplits
                                  , runTracker = (loadRun << .run) newSplits
                         }

loadRun : RunSpec -> RunTracker
loadRun runSpec = case runSpec of
    SingleCategorySpec segments -> SingleCategory <| loadSegments segments
    MultiCategorySpec  segments -> MultiCategory { previous = [], current = Nothing, upcoming = (List.map loadSegments segments) }

loadSegments : Run -> Run
loadSegments run =
    let splitset = .splits run
        (_, upcoming) = List.foldl buildSegmentSums (0, []) <| .upcoming splitset
    in { run | splits = { splitset | upcoming = upcoming } }

buildSegmentSums : Split -> (Int, List Split) -> (Int, List Split)
buildSegmentSums nextSplit (runningTime, splits) =
    let seg = .segment nextSplit
        pb = Maybe.map ((+) runningTime) <| .pb seg
        nextRunning = Maybe.withDefault runningTime pb
    in (nextRunning, splits ++ [{ nextSplit | segment = { seg | pb = pb } }])

start : Maybe Time -> Timer -> Timer
start time t =
    let currentTime = Just <| Maybe.withDefault (.currentTime <| splitsFor t) time
    in case t of
           Stopped s -> split currentTime <| Running { s | runStarted = Just <| .currentTime s }
           Paused  s -> Running s
           _         -> t

split : Maybe Time -> Timer -> Timer
split time t =
    let currentTime = Maybe.withDefault (.currentTime <| splitsFor t) time
    in case t of
        Running splits ->
            case (.runTracker splits) of
                SingleCategory _ ->
                    let newt = ((mapT << mapR << split_) currentTime) t
                    in case runFor newt of
                        Nothing -> newt -- This is required to make this function total but should be unreachable
                        Just r -> case (.current <| .splits r) of
                            Nothing -> let news = splitsFor newt in Finished { news | runEnded = Just <| .currentTime news }
                            Just _  -> newt
                MultiCategory rs ->
                    let newt = ((mapT << mapR << split_) currentTime) t
                        news = splitsFor newt
                    in case (.current rs) of
                        Nothing -> newt -- This is required to make this function total but should never be reached
                        Just r -> case (.current <| .splits r) of
                            Just _  -> newt
                            Nothing -> case (List.head <| .upcoming rs) of
                                Nothing ->
                                    let newRT = case (.runTracker news) of
                                                    MultiCategory nrs -> MultiCategory { nrs | previous = (.previous nrs) ++ [r]
                                                                                       , current  = Nothing
                                                                                       }
                                                    other -> other
                                    in Finished { news | runTracker = newRT }
                                next ->
                                    let newRT = case (.runTracker news) of
                                                    MultiCategory nrs -> MultiCategory { nrs | previous = (.previous nrs) ++ [r]
                                                                                       , current  = next
                                                                                       , upcoming = List.drop 1 (.upcoming nrs)
                                                                                       }
                                                    other -> other
                                    in Running { news | runTracker = newRT }
        _ -> t

split_ : Time -> Run -> Run
split_ t run =
    let splits        = .splits run
        prev          = .previous splits
        up            = .upcoming splits
        segmentDiff   = List.foldl (\candidate lastKnown -> if (.endTime candidate) == Nothing then lastKnown else (.endTime candidate)) (.runStarted run) prev
        singleSegment = Maybe.map (\t_ -> T.millisToPosix <| (T.posixToMillis t) - (T.posixToMillis t_)) segmentDiff
    in
        case up of
            [] -> case .current splits of
                Nothing -> run
                Just r  -> { run | splits   = { splits | previous = List.append prev [ { r | endTime = Just t, segmentTime = singleSegment  } ]
                                              ,          current = Nothing
                                              }
                                 , runEnded = Just t
                           }
            next :: rest -> case .current splits of
                Nothing -> { run | splits     = { splits | current = Just next
                                                ,          upcoming = rest
                                                }
                                 , runStarted = Just t
                           }
                Just r  -> { run | splits = { splits | previous = List.append prev [ { r | endTime = Just t, segmentTime = singleSegment } ]
                                            ,          current = Just next
                                            ,          upcoming = rest
                                            }
                           }

unsplit : Timer -> Timer
unsplit t = case mapT (mapR unsplit_) t of
    Finished r -> Running r
    r -> r

unsplit_ : Run -> Run
unsplit_ run =
    let splits = .splits run
        up = .upcoming splits
    in case List.reverse <| .previous splits of
        []      -> run
        x :: xs -> case .current splits of
            Nothing -> { run | splits = { splits | previous = List.reverse xs
                                        ,          current = Just { x | endTime = Nothing, segmentTime = Nothing }
                                        }
                       }
            Just s  -> { run | splits = { splits | previous = List.reverse xs
                                        ,          current = Just { x | endTime = Nothing, segmentTime = Nothing }
                                        ,          upcoming = { s | endTime = Nothing, segmentTime = Nothing } :: up
                                        }
                       }

skip : Timer -> Timer
skip = mapT (mapR skip_)

skip_ : Run -> Run
skip_ run =
    let splits = .splits run
        prev   = .previous splits
        up     = .upcoming splits
    in case up of
        [] ->
            run -- Cannot skip the final split of a run
        next :: rest -> case .current splits of
            Nothing -> run -- Cannot skip the first split of a run
            Just r  -> { run | splits = { splits | previous = List.append prev [ { r | endTime = Nothing, segmentTime = Nothing } ]
                                        ,          current = Just next
                                        ,          upcoming = rest
                                        }
                       }

splitsFor : Timer -> Splits
splitsFor timer = case timer of
    Stopped  s -> s
    Paused   s -> s
    Finished s -> s
    Running  s -> s

runFor : Timer -> Maybe Run
runFor timer = case .runTracker <| splitsFor timer of
    SingleCategory r -> Just r
    MultiCategory rs -> case .current rs of
                            Nothing -> List.head <| List.reverse <| .previous rs
                            r -> r

stop : Timer -> Timer
stop t = Stopped <| splitsFor t

reset : Timer -> Timer
reset = stop >> (mapT reset_)

reset_ : Splits -> Splits
reset_ splits = { splits | runStarted = Nothing, runEnded = Nothing, runTracker = resetRunTracker <| .runTracker splits }

resetRunTracker : RunTracker -> RunTracker
resetRunTracker runTracker = case runTracker of
    SingleCategory run -> SingleCategory <| resetRun run
    MultiCategory runs -> MultiCategory (resetRuns runs)

resetRun : Run -> Run
resetRun run = { run | splits = resetSplitSet <| .splits run }

resetRuns : RunSet -> RunSet
resetRuns runs = { previous = [], current = Nothing, upcoming = List.map resetRun <| allRuns runs }

resetSplitSet : SplitSet -> SplitSet
resetSplitSet set = { previous = [], current = Nothing, upcoming = List.map resetSplit <| allSplits set }

resetSplit : Split -> Split
resetSplit s = { s | endTime = Nothing, segmentTime = Nothing }

allRuns : RunSet -> List Run
allRuns set = case .current set of
    Nothing -> List.append (.previous set) (.upcoming set)
    Just r  -> List.concat [(.previous set), [r], (.upcoming set)]

allSplits : SplitSet -> List Split
allSplits set = case .current set of
    Nothing -> List.append (.previous set) (.upcoming set)
    Just s  -> List.concat [(.previous set), [s], (.upcoming set)]

toJSON : Timer -> JE.Value
toJSON timer = case (.runTracker << splitsFor) timer of
    SingleCategory run -> runToJSON run -- TODO add rest of Splits object context
    MultiCategory runs -> runsToJSON runs

runsToJSON : RunSet -> JE.Value
runsToJSON runs = JE.object [ ( "tag", JE.string "MultiCategoryRuns")
                            , ( "runs", JE.list runToJSON <| allRuns runs )
                            ]

runToJSON : Run -> JE.Value
runToJSON run =
    let category = case (.entityID << .category) run of
                       Nothing -> JE.null
                       Just c  -> JE.int c
        segments = List.filter (\x -> ((.entityID <| .segment x) /= Nothing) && ((.segmentTime x) /= Nothing) ) (allSplits <| .splits run)
    in
        JE.object [ ( "runCategory", category )
                  , ( "segments", JE.list (segmentsJSON (T.posixToMillis <| Maybe.withDefault epoch <| .runStarted run)) segments )
                  , ( "startTime", JE.int <| T.posixToMillis <| Maybe.withDefault epoch <| .runStarted run )
                  , ( "endTime", JE.int <| T.posixToMillis <| Maybe.withDefault epoch <| .runEnded run )
                  , ( "realTime", JE.int <| Maybe.withDefault 0 <| Maybe.map2 (\t1 t2 -> (T.posixToMillis t1) - (T.posixToMillis t2)) (.runEnded run) (.runStarted run) )
                  ]

segmentsJSON : Int -> Split -> JE.Value
segmentsJSON offset segment = case .entityID <| .segment segment of
    Nothing -> JE.null -- required to make the function total, filter before calling segmentsJSON
    Just i  -> JE.object [ ( "segment", JE.int i ), ( "time", segmentsJSON_ <| .segmentTime segment ) ]

segmentsJSON_ : Maybe Time -> JE.Value
segmentsJSON_ t = case t of
    Nothing -> JE.null
    Just i  -> JE.int <| T.posixToMillis i

{- TIME SYNC FUNCTIONS -}
setTime : Timer -> Time -> Timer
setTime timer t = mapT (\s -> { s | currentTime = t }) timer

{- DECODERS -}

checkTag : String -> JD.Decoder a -> JD.Decoder a
checkTag target da = checkTag_ (JD.at [ "data", "tag" ] JD.string) target da

checkTag2 : String -> JD.Decoder a -> JD.Decoder a
checkTag2 target da = checkTag_ (JD.at [ "data", "contents", "tag" ] JD.string) target da

checkTag_ : JD.Decoder String -> String -> JD.Decoder a -> JD.Decoder a
checkTag_ ds target da =
    ds |> JD.andThen (\s -> if s == target then da else JD.fail ("No match on tag:" ++ s))

gameDecoder : JD.Decoder Game
gameDecoder =
    JD.map4 Game ( JD.at [ "splitSetGameID" ] <| JD.maybe JD.int )
                 ( JD.at [ "splitSetGameData", "gameName" ] JD.string )
                 ( JD.at [ "splitSetGameData", "gameIcon" ] <| JD.nullable JD.string )
                 ( JD.at [ "splitSetGameData", "gameDefaultOffset" ] JD.int )

categoryDecoder : JD.Decoder Category
categoryDecoder =
    JD.map3 Category ( JD.at [ "splitSetCategoryID" ] <| JD.maybe JD.int )
                     ( JD.at [ "splitSetCategoryData", "categoryName" ] JD.string )
                     ( JD.at [ "splitSetCategoryData", "categoryOffset" ] JD.int )

segmentDecoder : JD.Decoder Segment
segmentDecoder =
    JD.map7 Segment ( JD.at [ "segmentID" ] <| JD.maybe JD.int )
                    ( JD.at [ "segmentName" ] JD.string )
                    ( JD.at [ "segmentIcon" ] <| JD.nullable JD.string )
                    ( JD.at [ "segmentPB" ] <| JD.nullable JD.int )
                    ( JD.at [ "segmentGold" ] <| JD.nullable JD.int )
                    ( JD.at [ "segmentAverage" ] <| JD.nullable JD.int )
                    ( JD.at [ "segmentWorst" ] <| JD.nullable JD.int )

segmentListDecoder : JD.Decoder SplitSet
segmentListDecoder = JD.at [ "splitSetSegments" ] <|
    JD.map (SplitSet [] Nothing) <| JD.list <|
        JD.map (\x -> Split x Nothing Nothing) segmentDecoder

runSpecDecoder : JD.Decoder RunSpec
runSpecDecoder = JD.oneOf [ singleRunSpecDecoder, multiRunSpecDecoder ]

runDecoder : JD.Decoder Run
runDecoder =
    JD.map3 (Run Nothing Nothing) ( JD.at [ "runGameData" ] gameDecoder )
                                  ( JD.at [ "runCategoryData" ] categoryDecoder )
                                  ( JD.at [ "runSplitsData" ] segmentListDecoder )

singleRunSpecDecoder : JD.Decoder RunSpec
singleRunSpecDecoder =
    checkTag_ ( JD.at [ "tag" ] JD.string) "SingleCategorySpec" <|
        JD.map SingleCategorySpec <| JD.at [ "runSpecData" ] runDecoder

multiRunSpecDecoder : JD.Decoder RunSpec
multiRunSpecDecoder =
    checkTag_ ( JD.at [ "tag" ] JD.string) "MultiCategorySpec" <|
        JD.map MultiCategorySpec <| JD.at [ "runSpecData" ] <| JD.list runDecoder
