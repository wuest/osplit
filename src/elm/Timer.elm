module Timer exposing ( Timer
                      , empty, start, stop, reset
                      , edit, view, subscriptions, update
                      , syncRequest, processSyncResponse
                      )

import Html            as H
import Html.Attributes as Attr
import Html.Events     as Event
import Time            as T
import Json.Encode     as JE

import Types exposing ( Msg(..), TimeSyncResponse )

import Debug as D

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

type alias Time = T.Posix
type alias Icon = Maybe String
type alias SplitSet = List Split

type alias Segment = { name      : String
                     , icon      : Icon
                     , pb        : Maybe Int
                     , gold      : Maybe Int
                     , average   : Maybe Int
                     , worst     : Maybe Int
                     }

type alias Split = { segment : Segment
                   , time    : Maybe Int
                   , change  : Maybe TimeChange
                   }

type alias Game = { name   : String
                  , icon   : Icon
                  , offset : Int
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

type alias SplitsListAccumulator = (Int, TimeSum, List (H.Html Msg))

{- BASE TIMER FUNCTIONS -}
empty : Timer
empty = Stopped { started   = zero
                , current   = zero
                , elapsed   = 0
                , game      = noGame
                , category  = "H'aanit"
                , passed    = [] -- TODO noSplits
                , split     = Just noSplit
                , remaining = noSplits
                }

elapsed : Timer -> Int
elapsed t = case t of
    Running s -> (.elapsed s) + ((millis <| .current s) - (millis <| .started s))
    Stopped s -> .elapsed s

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
                    Just s_ -> { s | split = Just { s_ | time = Nothing, change = Nothing }, remaining = List.map (\x -> { x | time = Nothing }) <| List.drop 1 <| .passed s, passed = [] }
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

{- FORMATTING -}
show : Int -> Timer -> String
show minSegments t = show_ minSegments (elapsed t) False

show_ : Int -> Int -> Bool -> String
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
        n ++
        (if minSegments > 3 || h > 0       then timerSegment h ++ ":" else "") ++
        (if minSegments > 2 || (h + m) > 0 then timerSegment m ++ ":" else "") ++
        timerSegment s ++ "." ++ (timerSegment <| u // 10)

game : Timer -> String
game = splitsFor >> .game >> .name

category : Timer -> String
category = splitsFor >> .category

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
                  [ H.text <| show 2 t ]
          , H.button [ Event.onClick <| StartSplitFinish (elapsed t) ] [ H.text "Start/Split" ]
          , H.button [ Event.onClick <| Unsplit (elapsed t) ] [ H.text "Unsplit" ]
          , H.button [ Event.onClick <| Stop (elapsed t) ] [ H.text "Stop" ]
          , H.button [ Event.onClick <| Reset (elapsed t) ] [ H.text "Reset" ]
          ]

timerClass : Timer -> String
timerClass t =
    case t of
        Stopped s -> if elapsed t == 0 then "neutral" else timerClass_ s <| elapsed t
        Running s ->
            case .split s of
                Nothing -> "neutral"
                Just s_ ->
                    let elapsedTime = (.elapsed s) + ((millis <| .current s) - (millis <| .started s)) -- this is duplicated a few places.  Please fix.
                        segmentTime = elapsedTime - (List.sum <| List.map (.time >> tfm) (.passed s))
                        splitStatus = timeStatus (elapsedTime, segmentTime) (pbSum (List.append (.passed s) [s_]), Just -1) -- Gold is set to impossible value
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

timerClass_ : Splits -> Int -> String
timerClass_ s elapsedTime =
    case .split s of
        Nothing ->
            case List.head <| List.drop ((List.length <| .passed s) - 1) (.passed s) of
                Nothing -> "neutral"
                Just s_ ->
                    let segmentTime = elapsedTime - (List.sum <| List.map (.time >> tfm) (.passed s))
                        splitStatus = timeStatus (elapsedTime, segmentTime) (pbSum (List.append (.passed s) [s_]), Just -1) -- Gold is set to impossible value
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
        Just s_ ->
            let segmentTime = elapsedTime - (List.sum <| List.map (.time >> tfm) (.passed s))
                splitStatus = timeStatus (elapsedTime, segmentTime) (pbSum (List.append (.passed s) [s_]), Just -1) -- Gold is set to impossible value
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

splitsListHeader : H.Html Msg
splitsListHeader =
    H.div [ Attr.id "split-listheader"
          , Attr.classList [("header", True), ("split", True)]
          ]
          [ H.div [ Attr.id <| "split-listheader-name", Attr.class "split-name" ] [ H.text "" ]
          , H.div [ Attr.id <| "split-listheader-pb", Attr.class "pb" ] [ H.text "+/- pb" ]
          , H.div [ Attr.id <| "split-listheader-gold", Attr.class "gold" ] [ H.text "+/- gold" ]
          , H.div [ Attr.id <| "split-listheader-average", Attr.class "average" ] [ H.text "+/- avg" ]
          , H.div [ Attr.id <| "split-listheader-worst", Attr.class "worst" ] [ H.text "+/- worst" ]
          , H.div [ Attr.id <| "split-listheader-split", Attr.class "split" ] [ H.text "" ]
          , H.div [ Attr.id <| "split-listheader-pb", Attr.class "running-pb" ] [ H.text "+/- pb" ]
          , H.div [ Attr.id <| "split-listheader-gold", Attr.class "running-gold" ] [ H.text "+/- gold" ]
          , H.div [ Attr.id <| "split-listheader-average", Attr.class "running-average" ] [ H.text "+/- avg" ]
          , H.div [ Attr.id <| "split-listheader-worst", Attr.class "running-worst" ] [ H.text "+/- worst" ]
          , H.div [ Attr.id <| "split-listheader-split", Attr.class "running-current" ] [ H.text "" ]
          ]

splitsList : Timer -> H.Html Msg
splitsList t =
    let s = splitsFor t
        r = elapsed t
        initSum = { pb = 0, gold = 0, average = 0, worst = 0, current = 0 }
        (n, times, hs) = List.foldl (splitsList_ Previous r) (0, initSum, []) (.passed s)
    in
        case .split s of
            Nothing   ->
                H.div [ Attr.id "splits-container" ] (splitsListHeader :: (List.reverse hs))
            Just next ->
                let (n_, times_, hs_)  = splitsList_ Current r next (n, times, hs)
                    (_, _, hs__) = List.foldl (splitsList_ Upcoming r) (n_, times_, hs_) (.remaining s)
                in
                  H.div [ Attr.id "splits-container" ] (splitsListHeader :: (List.reverse hs__))

splitsList_ : Position -> Int -> Split -> SplitsListAccumulator -> SplitsListAccumulator
splitsList_ relativePosition runningTime thisSplit (n, times, hs) =
    let pb   = tfm <| (.segment >> .pb) thisSplit
        g    = tfm <| (.segment >> .gold) thisSplit
        a    = tfm <| (.segment >> .average) thisSplit
        w    = tfm <| (.segment >> .worst) thisSplit
        s    = tfm <| .time thisSplit
        name = (.segment >> .name) thisSplit
        icon = (.segment >> .icon) thisSplit
        pos  = String.fromInt n
        nt   = { times | pb = (.pb times) + pb
               ,         gold = (.gold times) + g
               ,         average = (.average times) + a
               ,         worst = (.worst times) + w
               ,         current = (.current times) + s
               }
        div  = H.div [ Attr.id ("split-" ++ pos)
                     , Attr.classList <| List.concat [[("split", True), ("current", relativePosition == Current)], changeDisplay <| .change thisSplit]
                     ]
                     [ H.div [ Attr.id <| "split-" ++ pos ++ "-name", Attr.class "split-name" ] [ H.text <| (.segment >> .name) thisSplit ]
                     , runningDiv pos "pb" <| conditionalOffset relativePosition pb s
                     , runningDiv pos "gold" <| conditionalOffset relativePosition g s
                     , runningDiv pos "average" <| conditionalOffset relativePosition a s
                     , runningDiv pos "worst" <| conditionalOffset relativePosition w s
                     , runningDiv pos "split" <| show_ 2 s False

                     , runningDiv pos "running-pb" <| conditionalOffset relativePosition (.pb nt) (.current nt)
                     , runningDiv pos "running-gold" <| conditionalOffset relativePosition (.gold nt) (.current nt)
                     , runningDiv pos "running-average" <| conditionalOffset relativePosition (.average nt) (.current nt)
                     , runningDiv pos "running-worst" <| conditionalOffset relativePosition (.worst nt) (.current nt)
                     , runningDiv pos "running-current" <| conditionalOffset_ relativePosition (.current nt) runningTime
                     ]
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
                        _ -> [("ahead-split", True), ("losing-split", True)] -- Pattern match everything; gold is impossible here.

conditionalOffset : Position -> Int -> Int -> String
conditionalOffset pos segmentTime currentTime =
    case pos of
        Previous -> show_ 2 (currentTime - segmentTime) True
        _        -> show_ 2 segmentTime False

conditionalOffset_ : Position -> Int -> Int -> String
conditionalOffset_ pos segmentTime runningTime =
    case pos of
        Upcoming -> show_ 2 0 False
        Current  -> show_ 2 runningTime False
        Previous -> show_ 2 segmentTime False

runningDiv : String -> String -> String -> H.Html Msg
runningDiv id tag displayTime =
    H.div [ Attr.id <| "split-" ++ id ++ "-" ++ tag
          , Attr.class tag
          , Attr.class "segmentTime"
          ]
          [ H.text displayTime ]

{- SUBSCRIPTIONS -}
subscriptions : Sub Msg
subscriptions = Sub.batch [ T.every 1 Tick
                          , T.every (5 * minute) SyncTime
                          ]

{- UPDATE -}
update : Msg -> Timer -> Timer
update msg t = case msg of
    StartSplitFinish i -> start t i
    Stop _             -> stop t
    Reset _            -> reset t
    Unsplit _          -> unsplit t
    Tick tick          -> setTime t tick
    CloseSplits        -> empty
    _                  -> t

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

{- HELPER FUNCTIONS -}
millis : Time -> Int
millis = T.posixToMillis

time : Int -> Time
time = T.millisToPosix

zero : Time
zero = T.millisToPosix 0

noGame : Game
noGame = { name   = "Octopath Traveler"
         , icon   = Nothing
         , offset = 0
         }

noSplit : Split
noSplit = { segment = { name = "Ghisarma", icon = Nothing, pb = Just 99999999, gold = Just 1, average = Just 999999999, worst = Just 999999999 }, time = Nothing, change = Nothing }

noSplits : SplitSet
noSplits = [ { segment = { name = "Evasive Maneuvers", icon = Nothing, pb = Just 99999999, gold = Just 1, average = Just 99999999, worst = Just 99999999 }, time = Nothing, change = Nothing }
           , { segment = { name = "Merchant Shrine", icon = Nothing, pb = Just 99999999, gold = Just 1, average = Just 99999999, worst = Just 99999999 }, time = Nothing, change = Nothing }
           , { segment = { name = "Cyrus", icon = Nothing, pb = Just 99999999, gold = Just 1, average = Just 99999999, worst = Just 99999999 }, time = Nothing, change = Nothing }
           , { segment = { name = "Tressa", icon = Nothing, pb = Just 99999999, gold = Just 1, average = Just 99999999, worst = Just 99999999 }, time = Nothing, change = Nothing }
           , { segment = { name = "Therion", icon = Nothing, pb = Just 99999999, gold = Just 1, average = Just 99999999, worst = Just 99999999 }, time = Nothing, change = Nothing }
           , { segment = { name = "Enter Marsalim", icon = Nothing, pb = Just 99999999, gold = Just 1, average = Just 99999999, worst = Just 99999999 }, time = Nothing, change = Nothing }
           , { segment = { name = "Lord of the Forest", icon = Nothing, pb = Just 99999999, gold = Just 1, average = Just 99999999, worst = Just 99999999 }, time = Nothing, change = Nothing }
           , { segment = { name = "Dragon", icon = Nothing, pb = Just 99999999, gold = Just 1, average = Just 99999999, worst = Just 99999999 }, time = Nothing, change = Nothing }
           , { segment = { name = "Redeyes", icon = Nothing, pb = Just 99999999, gold = Just 1, average = Just 99999999, worst = Just 99999999 }, time = Nothing, change = Nothing }
           ]

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

timerSegment : Int -> String
timerSegment i = String.pad 2 '0' <| String.fromInt i

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
