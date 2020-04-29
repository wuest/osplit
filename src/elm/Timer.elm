module Timer exposing ( Timer
                      , empty, start, stop, reset
                      , edit, view, subscriptions, update, load
                      , syncRequest, processSyncResponse, toJSON
                      , isCompleted
                      )

import Timer.Types exposing ( .. )

import Html            as H
import Html.Attributes as Attr
import Html.Events     as Event
import Time            as T
import Json.Encode     as JE

import Types exposing ( Msg(..), TimeSyncResponse )

import Debug as D

type alias SplitsListAccumulator = (Int, TimeSum, List (H.Html Msg))

type alias Timer = Timer.Types.Timer

{- BASE TIMER FUNCTIONS -}
empty : Timer
empty = Stopped empty_

empty_ : Splits
empty_ = { started   = zero
         , current   = zero
         , elapsed   = 0
         , game      = noGame
         , category  = noCategory
         , passed    = noSplits
         , split     = noSplit
         , remaining = noSplits
         }

load : SplitsSpec -> Timer
load newSplits =
    let s     = List.head <| .segments newSplits
        rest_ = List.tail <| .segments newSplits
        rest  = case rest_ of
            Nothing -> []
            Just r  -> r
    in Stopped { empty_ | game = .game newSplits
                        , category = .category newSplits
                        , split = s
                        , remaining = rest
               }

elapsed : Timer -> Int
elapsed t = case t of
    Running s -> (.elapsed s) + ((millis <| .current s) - (millis <| .started s))
    Stopped s -> .elapsed s

isCompleted : Timer -> Bool
isCompleted t = case t of
    Running _ -> False
    Stopped s -> (.split s) == Nothing

start : Timer -> Int -> Timer
start t i = case t of
    Running s -> split t i
    Stopped s ->
        case .split s of
            Nothing -> t
            Just s_ -> Running { s | started = .current s }

stop : Timer -> Timer
stop t = case t of
    Running s -> Stopped { s | elapsed = elapsed t }
    Stopped _ -> t

reset : Timer -> Timer
reset t = let s = resetSplitsFor t
          in Stopped { s | started = time 0, current = time 0, elapsed = 0 }

resetSplitsFor : Timer -> Splits
resetSplitsFor t =
    let s = splitsFor t
    in
        case .split s of
            Nothing ->
                case List.head (.passed s) of
                    Nothing -> s
                    Just s_ -> { s | split = Just { s_ | time = Nothing, change = Nothing }, remaining = List.map (\x -> { x | time = Nothing, change = Nothing }) <| List.drop 1 <| .passed s, passed = [] }
            Just s_ ->
                let fullList = List.map (\x -> { x | time = Nothing, change = Nothing }) <| List.concat [.passed s, [s_], .remaining s]
                in
                    case fullList of
                        fst :: rst -> { s | split = Just fst, remaining = rst, passed = [] }
                        []         -> s -- This branch should never be reached by god or man

setTime : Timer -> T.Posix -> Timer
setTime t p = case t of
    Running s -> Running { s | current = p }
    Stopped s -> Stopped { s | current = p }

split : Timer -> Int -> Timer
split t i = case t of
    Running s -> split_ s i
    Stopped s -> t

split_ : Splits -> Int -> Timer
split_ s i =
    case .split s of
        Nothing -> stop <| Running s
        Just s_ -> let elapsedTime = (.elapsed s) + ((millis <| .current s) - (millis <| .started s)) -- this is duplicated a few places.  Please fix.
                       segmentTime = elapsedTime - (List.sum <| List.map (.time >> tfm) (.passed s))
                       splitStatus = timeStatus (elapsedTime, segmentTime) (pbSum (List.append (.passed s) [s_]), (.segment >> .gold) s_)
                       splitChange = timeChange splitStatus segmentTime <| (.segment >> .pb >> tfm) s_
                       priorSplit = { s_ | time = Just segmentTime, change = Just splitChange }
                       newPassed = List.append (.passed s) [priorSplit]
                       remaining = .remaining s
                   in
                       case List.head remaining of
                           Nothing -> stop <| Running { s | passed = newPassed, split = Nothing }
                           next    ->         Running { s | passed = newPassed, split = next, remaining = List.drop 1 remaining }

unsplit : Timer -> Timer
unsplit t = case t of
    Running s -> Running <| unsplit_ s
    Stopped s -> Running <| unsplit_ s

unsplit_ : Splits -> Splits
unsplit_ s =
    case List.reverse <| .passed s of
        []      -> s
        x :: xs -> case .split s of
            Nothing -> { s | split     = Just { x | time = Nothing, change = Nothing }
                       ,     passed    = List.reverse xs
                       ,     elapsed   = .elapsed s - (tfm <| .time x)
                       }
            Just s_ -> { s | split     = Just { x | time = Nothing, change = Nothing }
                       ,     passed    = List.reverse xs
                       ,     remaining = s_ :: (.remaining s)
                       }

skip : Timer -> Timer
skip t = case t of
    Running s -> skip_ s
    Stopped s -> t

skip_ : Splits -> Timer
skip_ s =
    case .split s of
        Nothing -> Running s -- Cannot skip final split
        Just s_ -> let skippedSplit = { s_ | change = Just Skipped }
                       newPassed    = List.append (.passed s) [skippedSplit]
                       remaining    = .remaining s
                   in
                       case List.head remaining of
                           Nothing -> Running s -- Cannot skip final split
                           next    -> Running { s | passed = newPassed, split = next, remaining = List.drop 1 remaining }

{- FORMATTING -}
show : Int -> Timer -> H.Html Msg
show minSegments t = show_ minSegments (elapsed t) False

show_ : Int -> Int -> Bool -> H.Html Msg
show_ minSegments uSeconds showSign =
    let n  = if uSeconds < 0 then "-" else
                 if showSign == True then "+" else ""
        e  = abs uSeconds
        h  = e // hour
        hr = e - (h * hour)
        m  = hr // minute
        mr = hr - (m * minute)
        s  = mr // second
        u  = mr - (s * second)
    in
        H.div [ Attr.class "time-readout" ]
              ( List.concat [ [(H.div [ Attr.class "time-sign" ] [ H.text n ])]
                            , clockSegments minSegments h m s u
                            ]
              )

clockSegments : Int -> Int -> Int -> Int -> Int -> List (H.Html Msg)
clockSegments minSegments h m s u =
    let hD = if minSegments > 3 || h > 0 then [ H.div [ Attr.class "time-hour" ] [ H.text <| timerSegment False h ] ] else []
        mD = if minSegments > 2 || (h + m) > 0 then [ H.div [ Attr.class "time-minute" ] [ H.text <| timerSegment (h > 0 || minSegments > 3) m ] ] else []
        sD = [ H.div [ Attr.class "time-second" ] [ H.text <| timerSegment True s ] ]
        uD = [ H.div [ Attr.class "time-usecond" ] [ H.text <| timerSegment True <| u // 10 ] ]
    in
        List.concat [List.intersperse divC <| List.concat [hD, mD, sD], [divD], uD]

divC : H.Html Msg
divC = H.div [ Attr.class "time-colon" ] [ H.text ":" ]

divD : H.Html Msg
divD = H.div [ Attr.class "time-decimal" ] [ H.text "." ]

game : Timer -> String
game = splitsFor >> .game >> .name

category : Timer -> String
category = splitsFor >> .category >> .name

hour : number
hour = 3600000 -- 60 minutes * 60 seconds * 1000 milliseconds

minute : number
minute = 60000 -- 60 seconds * 1000 milliseconds

second : number
second = 1000 -- 1000 milliseconds

{- VIEW -}
edit : Timer -> H.Html Msg
edit = view

view : Timer -> H.Html Msg
view t =
    H.div [ Attr.id "timer-container" ]
          [ H.div [ Attr.id "timer-title" ]
                  [ H.text <| game t ]
          , H.div [ Attr.id "timer-category" ]
                  [ H.text <| category t ]
          , splitsList t
          , H.div [ Attr.id "main-timer"
                  , Attr.class <| timerClass t
                  ]
                  [ show 3 t ]
          , H.button [ Event.onClick <| StartSplit (elapsed t) ] [ H.text "Start/Split" ]
          , H.button [ Event.onClick <| Unsplit (elapsed t) ] [ H.text "Unsplit" ]
          , H.button [ Event.onClick <| Skip (elapsed t) ] [ H.text "Skip" ]
          , H.button [ Event.onClick <| Stop (elapsed t) ] [ H.text "Stop" ]
          , H.button [ Event.onClick Reset ] [ H.text "Reset" ]
          ]

timerClass : Timer -> String
timerClass t =
    case t of
        Stopped s -> if elapsed t == 0 then "neutral" else timerClass_ s <| elapsed t
        Running s -> timerClass_ s <| elapsed t

timerClass_ : Splits -> Int -> String
timerClass_ s elapsedTime =
    case .split s of
        Nothing ->
            case List.head <| List.drop ((List.length <| .passed s) - 1) (.passed s) of
                Nothing -> "neutral"
                Just s_ ->
                    let segmentTime = elapsedTime - (List.sum <| List.map (.time >> tfm) (.passed s))
                        splitStatus = timeStatus (elapsedTime, segmentTime) (pbSum (List.append (.passed s) [s_]), (.segment >> .gold) s_)
                        splitChange = timeChange splitStatus segmentTime <| (.segment >> .pb >> tfm) s_
                    in
                        case splitChange of
                            Gaining x ->
                                case x of
                                    Behind -> "gaining-behind"
                                    _ -> "gaining-ahead"
                            Losing x ->
                                case x of
                                    Behind -> "losing-behind"
                                    _ -> "losing-ahead"
                            _ -> "impossible"
        Just s_ ->
            let segmentTime = elapsedTime - (List.sum <| List.map (.time >> tfm) (.passed s))
                splitStatus = timeStatus (elapsedTime, segmentTime) (pbSum (List.append (.passed s) [s_]), (.segment >> .gold) s_)
                splitChange = timeChange splitStatus segmentTime <| (.segment >> .pb >> tfm) s_
            in
                case splitChange of
                    Gaining x ->
                        case x of
                            Behind -> "gaining-behind"
                            _ -> "gaining-ahead"
                    Losing x ->
                        case x of
                            Behind -> "losing-behind"
                            _ -> "losing-ahead"
                    _ -> "impossible"

splitsListHeader : H.Html Msg
splitsListHeader =
    H.div [ Attr.id "split-listheader"
          , Attr.classList [("header", True), ("split", True)]
          ]
          [ H.div [ Attr.id <| "split-listheader-name", Attr.classList [("split-column", True), ("split-name", True)] ] [ H.text "" ]
          , H.div [ Attr.id <| "split-listheader-pb", Attr.classList [("split-column", True), ("pb", True)] ] [ H.text "+/- pb" ]
          , H.div [ Attr.id <| "split-listheader-gold", Attr.classList [("split-column", True), ("gold", True)] ] [ H.text "+/- gold" ]
          , H.div [ Attr.id <| "split-listheader-average", Attr.classList [("split-column", True), ("average", True)] ] [ H.text "+/- avg" ]
          , H.div [ Attr.id <| "split-listheader-worst", Attr.classList [("split-column", True), ("worst", True)] ] [ H.text "+/- worst" ]
          , H.div [ Attr.id <| "split-listheader-split", Attr.classList [("split-column", True), ("split", True)] ] [ H.text "" ]
          , H.div [ Attr.id <| "split-listheader-pb", Attr.classList [("split-column", True), ("running-pb", True)] ] [ H.text "+/- pb" ]
          , H.div [ Attr.id <| "split-listheader-gold", Attr.classList [("split-column", True), ("running-gold", True)] ] [ H.text "+/- gold" ]
          , H.div [ Attr.id <| "split-listheader-average", Attr.classList [("split-column", True), ("running-average", True)] ] [ H.text "+/- avg" ]
          , H.div [ Attr.id <| "split-listheader-worst", Attr.classList [("split-column", True), ("running-worst", True)] ] [ H.text "+/- worst" ]
          , H.div [ Attr.id <| "split-listheader-split", Attr.classList [("split-column", True), ("running-current", True)] ] [ H.text "" ]
          ]

splitsList : Timer -> H.Html Msg
splitsList t =
    let s = splitsFor t
        r = elapsed t
        initSum = { pb = 0, gold = Just 0, average = 0, worst = 0, current = 0 }
        (n, times, hs) = List.foldl (splitsList_ Previous r) (0, initSum, []) (.passed s)
        currentStatus = case t of Running _ -> Current
                                  Stopped _ -> Upcoming
    in
        case .split s of
            Nothing   ->
                H.div [ Attr.id "splits-container" ] (splitsListHeader :: (List.reverse hs))
            Just next ->
                let (n_, times_, hs_)  = splitsList_ currentStatus r next (n, times, hs)
                    (_, _, hs__) = List.foldl (splitsList_ Upcoming r) (n_, times_, hs_) (.remaining s)
                in
                  H.div [ Attr.id "splits-container" ] (splitsListHeader :: (List.reverse hs__))

splitsList_ : Position -> Int -> Split -> SplitsListAccumulator -> SplitsListAccumulator
splitsList_ relativePosition runningTime thisSplit (n, times, hs) =
    let pb   =        (.segment >> .pb) thisSplit
        g    =        (.segment >> .gold) thisSplit
        a    = tfm <| (.segment >> .average) thisSplit
        w    = tfm <| (.segment >> .worst) thisSplit
        s    = tfm <| .time thisSplit
        name = (.segment >> .name) thisSplit
        icon = (.segment >> .icon) thisSplit
        pos  = String.fromInt n
        nt   = { times | pb = (.pb times) + (tfm pb)
               ,         gold = liftA2 (+) (.gold times) g
               ,         average = (.average times) + a
               ,         worst = (.worst times) + w
               ,         current = (.current times) + s
               }
        cpb  = fmap ((+) (.pb times)) pb
        divContent = case .change thisSplit of
                         Just Skipped ->
                             [ H.div [ Attr.id <| "split-" ++ pos ++ "-name", Attr.class "split-name" ] [ H.text <| (.segment >> .name) thisSplit ]
                             , runningDiv pos "pb" <| H.div [ Attr.class "time-readout" ] [ H.text "-" ]
                             , runningDiv pos "gold" <| H.div [ Attr.class "time-readout" ] [ H.text "-" ]
                             , runningDiv pos "average" <| H.div [ Attr.class "time-readout" ] [ H.text "-" ]
                             , runningDiv pos "worst" <| H.div [ Attr.class "time-readout" ] [ H.text "-" ]
                             , runningDiv pos "split" <| H.div [ Attr.class "time-readout" ] [ H.text "-" ]

                             , runningDiv pos "running-pb" <| H.div [ Attr.class "time-readout" ] [ H.text "-" ]
                             , runningDiv pos "running-gold" <| H.div [ Attr.class "time-readout" ] [ H.text "-" ]
                             , runningDiv pos "running-average" <| H.div [ Attr.class "time-readout" ] [ H.text "-" ]
                             , runningDiv pos "running-worst" <| H.div [ Attr.class "time-readout" ] [ H.text "-" ]
                             , runningDiv pos "running-current" <| H.div [ Attr.class "time-readout" ] [ H.text "-" ]
                             ]
                         _ ->
                             [ H.div [ Attr.id <| "split-" ++ pos ++ "-name", Attr.class "split-name" ] [ H.text <| (.segment >> .name) thisSplit ]
                             , runningDiv pos "pb" <| conditionalOffsetM relativePosition pb s
                             , runningDiv pos "gold" <| conditionalOffsetM relativePosition g s
                             , runningDiv pos "average" <| conditionalOffset relativePosition a s
                             , runningDiv pos "worst" <| conditionalOffset relativePosition w s
                             , runningDiv pos "split" <| show_ 2 s False

                             , runningDiv pos "running-pb" <| conditionalOffsetM relativePosition cpb (.current nt)
                             , runningDiv pos "running-gold" <| conditionalOffsetM relativePosition (.gold nt) (.current nt)
                             , runningDiv pos "running-average" <| conditionalOffset relativePosition (.average nt) (.current nt)
                             , runningDiv pos "running-worst" <| conditionalOffset relativePosition (.worst nt) (.current nt)
                             , runningDiv pos "running-current" <| conditionalOffset_ relativePosition (.current nt) runningTime
                             ]
        div  = H.div [ Attr.id ("split-" ++ pos)
                     , Attr.classList <| List.concat [[("split", True), ("current", relativePosition == Current)], changeDisplay <| .change thisSplit]
                     ]
                     divContent
    in
        (n + 1, nt, div :: hs)

changeDisplay : Maybe TimeChange -> List (String, Bool)
changeDisplay maybeChange =
    case maybeChange of
        Nothing -> []
        Just change ->
            case change of
                Gaining x ->
                    case x of
                        Behind -> [("behind-split", True), ("gaining-split", True)]
                        Ahead -> [("ahead-split", True), ("gaining-split", True)]
                        Gold -> [("gold-split", True), ("gaining-split", True)]
                Losing x ->
                    case x of
                        Behind -> [("behind-split", True), ("losing-split", True)]
                        Ahead -> [("ahead-split", True), ("losing-split", True)]
                        Gold -> [("gold-split", True), ("gaining-split", True)] -- This occurs when there is no gold for the split
                Skipped -> [("skipped-split", True)]

conditionalOffset : Position -> Int -> Int -> H.Html Msg
conditionalOffset pos segmentTime currentTime =
    case pos of
        Previous -> show_ 2 (currentTime - segmentTime) True
        _        -> show_ 2 segmentTime False

conditionalOffset_ : Position -> Int -> Int -> H.Html Msg
conditionalOffset_ pos segmentTime runningTime =
    case pos of
        Upcoming -> H.div [] []
        Current  -> show_ 2 runningTime False
        Previous -> show_ 2 segmentTime False

conditionalOffsetM : Position -> Maybe Int -> Int -> H.Html Msg
conditionalOffsetM pos segmentTime currentTime =
    case segmentTime of
        Nothing -> H.div [] []
        Just s  -> conditionalOffset pos s currentTime

runningDiv : String -> String -> H.Html Msg -> H.Html Msg
runningDiv splitID tag displayTime =
    H.div [ Attr.id <| "split-" ++ splitID ++ "-" ++ tag
          , Attr.classList [("split-column", True), (tag, True), ("segmentTime", True)]
          ]
          [ displayTime ]

{- SUBSCRIPTIONS -}
subscriptions : Sub Msg
subscriptions = Sub.batch [ T.every 1 Tick
                          , T.every (5 * minute) SyncTime
                          ]

{- UPDATE -}
update : Msg -> Timer -> Timer
update msg t = case msg of
    StartSplit i -> start t i
    Unsplit _    -> unsplit t
    Skip _       -> skip t
    Stop _       -> stop t
    Reset        -> reset t
    Tick tick    -> setTime t tick
    CloseSplits  -> empty
    _            -> t

{- TIME SYNC -}
syncRequest : Timer -> JE.Value
syncRequest t =
    let currentTime = (splitsFor >> .current >> millis) t in
        JE.object [ ( "tag", JE.string "TimeSyncInit" )
                  , ( "contents"
                    , JE.object [ ( "currentTime", JE.int currentTime )
                                , ( "previousOffset", JE.null )
                                ]
                    )
                  ]
--                  {"currentTime":0,            "previousOffset":null}
--                  {"currentTime":1582599029907,"previousOffset":null}
--                  {"currentTime":1582599150230,"previousOffset":null}

processSyncResponse : Timer -> TimeSyncResponse -> Int
processSyncResponse t response =
    let currentTime = (splitsFor >> .current >> millis) t in
        ((currentTime - (.currentTime response)) + (.previousOffset response)) // 2

toJSON : Timer -> JE.Value
toJSON t =
    let pull ms = case ms of
            Nothing -> []
            Just js -> [js]
        cat = case (splitsFor >> .category >> .entityID) t of
            Nothing -> JE.null
            Just c  -> JE.int c
        splits = splitsFor t
        p = .passed splits
        s = pull <| .split splits
        r = .remaining splits
        allSplits =  p ++ s ++ r
        startTime = (.started >> millis) splits
        realTime  = elapsed t
        endTime   = startTime + realTime
    in
        JE.object [ ( "runCategory", cat )
                  , ( "segments", JE.list segmentsJSON <| List.filter (\x -> ((.entityID <| .segment x) /= Nothing) && ((.time x) /= Nothing) ) allSplits )
                  , ( "startTime", JE.int startTime )
                  , ( "realTime", JE.int realTime )
                  , ( "endTime", JE.int endTime )
                  ]

segmentsJSON : Split -> JE.Value
segmentsJSON s =
    case .entityID <| .segment s of
        Just i  -> JE.object [ ("segment", JE.int i)
                             , ("time", segmentsJSON_ <| .time s)
                             ]
        Nothing -> JE.null -- unreachable

segmentsJSON_ : Maybe Int -> JE.Value
segmentsJSON_ x =
    case x of
        Just y  -> JE.int y
        Nothing -> JE.null -- unreachable

{- HELPER FUNCTIONS -}
millis : Time -> Int
millis = T.posixToMillis

time : Int -> Time
time = T.millisToPosix

zero : Time
zero = T.millisToPosix 0

noGame : Game
noGame = { entityID = Nothing
         , name     = ""
         , icon     = Nothing
         , offset   = 0
         }

noSplit : Maybe Split
noSplit = Just { segment = { entityID = Just -1, name = "", icon = Nothing, pb = Nothing, gold = Nothing, average = Nothing, worst = Nothing }, time = Nothing, change = Nothing }

noSplits : SplitSet
noSplits = [ ]

noCategory : Category
noCategory = { entityID = Nothing
             , name = ""
             , offset = 0
             }

splitsFor : Timer -> Splits
splitsFor t = case t of
    Running t_ -> t_
    Stopped t_ -> t_

-- Enumerable#detect from ruby, more or less, but gives an index too
detect : (a -> Bool) -> List a -> Maybe (Int, a)
detect p xs = detect_ 0 p xs

detect_ : Int -> (a -> Bool) -> List a -> Maybe (Int, a)
detect_ i p xs = case xs of
    [] -> Nothing
    x :: xs_ -> if p x
                then Just (i, x)
                else detect_ (i+1) p xs_

-- Fetch a pivot point, return a tuple of before, pivot, after
partitionAt : Int -> List a -> Maybe (List a, a, List a)
partitionAt i xs =
    let a = List.take i xs
        b = List.head <| List.drop i xs
        c = List.drop (i + 1) xs
    in
        case b of
            Just b_ -> Just (a, b_, c)
            Nothing -> Nothing

timerSegment : Bool -> Int -> String
timerSegment padTime i =
    if padTime then String.pad 2 '0' <| String.fromInt i else String.fromInt i

-- "time from maybe"
tfm : Maybe Int -> Int
tfm mt = case mt of
    Nothing -> 0
    Just t  -> t

timeStatus : (Int, Int) -> (Maybe Int, Maybe Int) -> TimeStatus
timeStatus (elapsedTotal, segmentTime) (compareTotal, goldTime) =
    case goldTime of
        Nothing -> Gold
        Just gt ->
            if gt > segmentTime
            then Gold
            else case compareTotal of
                Nothing -> Ahead
                Just ct ->
                    if elapsedTotal <= ct
                    then Ahead
                    else Behind

pbSum : List Split -> Maybe Int
pbSum xs =
    let s = List.sum <| List.map (.segment >> .pb >> tfm) xs in
        if s == 0 then Nothing else Just s

timeChange : TimeStatus -> Int -> Int -> TimeChange
timeChange status current pb =
    if current <= pb
    then Gaining status
    else Losing status

fmap : (a -> b) -> Maybe a -> Maybe b
fmap f ma =
    case ma of
        Nothing -> Nothing
        Just a  -> Just (f a)

liftA2 : (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
liftA2 f ma mb =
    case ma of
        Nothing -> Nothing
        Just a ->
            case mb of
                Nothing -> Nothing
                Just b -> Just <| f a b
