{-# LANGUAGE DeriveGeneric #-}

module Message where

import Prelude
import GHC.Generics  ( Generic )

import qualified Data.Aeson as JSON
import qualified Data.Text  as Text

import qualified Data.Model as Model

data TimeState = TimeState { currentTime  :: Int
                           , previousOffset :: Maybe Int
                           } deriving ( Show, Generic )

type ConfigKey = Text.Text
type ConfigVal = Text.Text

data SplitsCommand = RemoteStartSplit Int
                   | RemoteFinish SplitSet
                   | RemoteUnsplit Int
                   | RemoteSkip Int
                   | RemoteStop Int
                   | RemoteReset SplitSet
    deriving ( Show, Generic )

data MenuCommand = MenuGames
                 | MenuCategories Int
                 | MenuLoadSplits Int
                 | MenuCloseSplits
    deriving ( Show, Generic )

data Command = TimeSyncInit TimeState
             | NewClient
             | TimerControl SplitsCommand
             | Menu MenuCommand
             | SetConfig ConfigKey ConfigVal
             | FetchConfig ConfigKey
    deriving ( Show, Generic )

data SplitSet = SplitSet { runCategory :: Int
                         , segments    :: [SegmentTime]
                         , startTime   :: Int
                         , endTime     :: Int
                         , realTime    :: Int
                         } deriving ( Show, Generic )

data SegmentTime = SegmentTime { segment :: Int
                               , time    :: Int
                               } deriving ( Show, Generic )

data Game = Game { gameID   :: Int
                 , gameData :: Model.Game
                 } deriving ( Show, Generic )

data Category = Category { categoryID   :: Int
                         , categoryData :: Model.Category
                         } deriving ( Show, Generic )

data Response = Raw { respType :: Text.Text
                    , respData :: Text.Text
                    }
              | TimeSyncResponse TimeState
              | RemoteControl SplitsCommand
              | GameList [Game]
              | CategoryList [Category]
              | SplitsRefresh (Maybe LoadedSplits)
              | CloseSplits
              | ConfigStore ConfigKey ConfigVal
              deriving ( Show, Generic )

data SegmentData = SegmentData { segmentID      :: Int
                               , segmentName    :: Text.Text
                               , segmentIcon    :: Maybe Text.Text
                               , segmentPB      :: Maybe Int
                               , segmentGold    :: Maybe Int
                               , segmentAverage :: Maybe Int
                               , segmentWorst   :: Maybe Int
                               } deriving ( Show, Generic )

data LoadedSplits = LoadedSplits { splitSetGameID :: Int
                                 , splitSetGameData :: Model.Game
                                 , splitSetCategoryID :: Int
                                 , splitSetCategoryData :: Model.Category
                                 , splitSetSegments :: [SegmentData]
                                 } deriving ( Show, Generic )

instance JSON.FromJSON SplitsCommand
instance JSON.ToJSON SplitsCommand
instance JSON.FromJSON MenuCommand
instance JSON.ToJSON MenuCommand
instance JSON.FromJSON TimeState
instance JSON.ToJSON TimeState
instance JSON.FromJSON SegmentTime
instance JSON.ToJSON SegmentTime
instance JSON.FromJSON Command
instance JSON.ToJSON Command
instance JSON.ToJSON Response
instance JSON.FromJSON Category
instance JSON.ToJSON Category
instance JSON.FromJSON Game
instance JSON.ToJSON Game
instance JSON.FromJSON SplitSet
instance JSON.ToJSON SplitSet
instance JSON.FromJSON SegmentData
instance JSON.ToJSON SegmentData
instance JSON.FromJSON LoadedSplits
instance JSON.ToJSON LoadedSplits
