{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults #-}

module Data.LSS where

import Prelude

import Data.Aeson       ( FromJSON, ToJSON )
import GHC.Generics     ( Generic )
import Data.List.Split  ( splitOn )
import Data.Time.Clock  ( DiffTime, UTCTime, picosecondsToDiffTime )
import Data.Time.Format ( parseTimeM, defaultTimeLocale )

import Text.XML.HXT.Core

data Attempt = Attempt { attemptId :: Integer
                       , startTime :: UTCTime
                       , startSynced :: Bool
                       , endTime :: UTCTime
                       , endSynced :: Bool
                       , realTime :: Maybe DiffTime
                       } deriving ( Show, Generic )
instance FromJSON Attempt
instance ToJSON Attempt

data SplitTime = SplitTime { splitTimeName :: String
                           , splitRealTime :: DiffTime
                           } deriving ( Show, Generic )
instance FromJSON SplitTime
instance ToJSON SplitTime

data SegmentHistory = SegmentHistory { timeId :: Integer
                                     , segmentRealTime :: Maybe DiffTime
                                     } deriving ( Show, Generic )
instance FromJSON SegmentHistory
instance ToJSON SegmentHistory

data Segment = Segment { segmentName :: String
                       , icon :: String
                       , splitTimes :: [SplitTime]
                       , bestSegmentTime :: DiffTime
                       , segmentHistory :: [SegmentHistory]
                       } deriving ( Show, Generic )
instance FromJSON Segment
instance ToJSON Segment

data Run = Run { gameIcon :: String
               , gameName :: String
               , categoryName :: String
               , attemptCount :: Integer
               , offset :: DiffTime
               , attemptHistory :: [Attempt]
               , segments :: [Segment]
               } deriving ( Show, Generic )
instance FromJSON Run
instance ToJSON Run

parseXML x = runX (readDocument [ withValidate no, withRemoveWS yes ] x >>> getRun)

atTag tag = deep (isElem >>> hasName tag)
text = getChildren >>> getText
ensureText tag = (atTag tag >>> text) `orElse` arr (const "")

parseTime :: String -> DiffTime
parseTime s =
    let parts = splitOn ":" s
        pos = if head (head parts) == '-' then (-1) else 1
        total = fromIntegral pos
                * fromIntegral (3600 * (read $ head parts :: Integer))
                + fromIntegral (60 * (read $ parts !! 1 :: Integer))
                + (read $ parts !! 2 :: Float)
    in
    picosecondsToDiffTime $ round (total * 1000000000000)

getMaybeRealTime :: String -> Maybe DiffTime
getMaybeRealTime "" = Nothing
getMaybeRealTime s = Just $ parseTime s

parseDateTime :: String -> UTCTime
parseDateTime = parseDateTime' . parseTimeM True defaultTimeLocale "%m/%d/%Y %H:%M:%S"

parseDateTime' :: Maybe UTCTime -> UTCTime
parseDateTime' (Just t) = t
parseDateTime' Nothing = parseDateTime "01/01/2018 00:00:00"

getAttempts =
    atTag "Attempt" >>>
        proc h -> do
            a <- getAttrValue "id" -< h
            st <- getAttrValue "started" -< h
            ss <- getAttrValue "isStartedSynced" -< h
            et <- getAttrValue "ended" -< h
            es <- getAttrValue "isEndedSynced" -< h
            rt <- ensureText "RealTime" -< h
            returnA -< Attempt { attemptId = read a
                               , startTime = parseDateTime st
                               , startSynced = ss == "True"
                               , endTime = parseDateTime et
                               , endSynced = es == "True"
                               , realTime = getMaybeRealTime rt
                               }

getSplitTimes =
    atTag "SplitTime" >>>
        proc s -> do
            n <- getAttrValue "name" -< s
            t <- ensureText "RealTime" -< s
            returnA -< SplitTime { splitTimeName = n
                                 , splitRealTime = parseTime t
                                 }

getSegmentHistory =
    atTag "Time" >>>
        proc t -> do
            i <- getAttrValue "id" -< t
            r <- ensureText "RealTime" -< t
            returnA -< SegmentHistory { timeId = read i
                                      , segmentRealTime = getMaybeRealTime r
                                      }

getSegments =
    atTag "Segment" >>>
        proc s -> do
            n <- ensureText "Name" -< s
            i <- ensureText "Icon" -< s
            b <- ensureText "RealTime" <<< atTag "BestSegmentTime" -< s
            st <- listA getSplitTimes -< s
            sh <- listA getSegmentHistory -< s
            returnA -< Segment { segmentName = n
                               , icon = i
                               , splitTimes = st
                               , bestSegmentTime = parseTime b
                               , segmentHistory = sh
                               }

getRun =
    atTag "Run" >>>
        proc r -> do
            i <- ensureText "GameIcon" -< r
            g <- ensureText "GameName" -< r
            c <- ensureText "CategoryName" -< r
            o <- ensureText "Offset" -< r
            a <- ensureText "AttemptCount" -< r
            h <- listA getAttempts -< r
            s <- listA getSegments -< r
            returnA -< Run { gameIcon = i
                           , gameName = g
                           , categoryName = c
                           , offset = parseTime o
                           , attemptCount = read a
                           , attemptHistory = h
                           , segments = s
                           }
