{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Message where

import Prelude
import GHC.Generics  ( Generic )

import qualified Data.Aeson as JSON
import qualified Data.Text  as Text

import qualified Data.Model as Model

import Data.Aeson ( FromJSON(..), (.:), (.:?), withObject )

data TimeState = TimeState { currentTime  :: Int
                           , previousOffset :: Maybe Int
                           } deriving ( Show, Generic )

type ConfigKey = Text.Text
type ConfigVal = Text.Text

data SplitsCommand = RemoteStartSplit Int
                   | RemoteFinish SplitSet
                   | RemoteUnsplit
                   | RemoteSkip
                   | RemoteStop
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
             | NewSplits NewSplitsSpec
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

{- Here's the new stuff! -}

data GameSpec = GameSpec { name :: Text.Text
                         , icon :: Maybe Text.Text
                         , offset :: Integer
                         } deriving ( Show, Generic )

instance FromJSON GameSpec where
    parseJSON = withObject "GameSpec" $ \o -> do
        name <- o .: "name"
        icon <- o .:? "icon"
        offset <- o .: "offset"
        return GameSpec{..}

data CategorySpec = CategorySpec { name :: Text.Text
                                 , offset :: Integer
                                 } deriving ( Show, Generic )

instance FromJSON CategorySpec where
    parseJSON = withObject "CategorySpec" $ \o -> do
        name <- o .: "name"
        offset <- o .: "offset"
        return CategorySpec{..}

data SegmentSpec = SegmentSpec { name :: Text.Text
                               , icon :: Maybe Text.Text
                               } deriving ( Show, Generic )

instance FromJSON SegmentSpec where
    parseJSON = withObject "GameSpec" $ \o -> do
        name <- o .: "name"
        icon <- o .:? "icon"
        return SegmentSpec{..}

data NewSplitsSpec = NewSplitsSpec { title :: Text.Text
                                   , subtitle :: Text.Text
                                   , game :: GameSpec
                                   , category :: CategorySpec
                                   , segments :: [SegmentSpec]
                                   } deriving ( Show, Generic )

instance FromJSON NewSplitsSpec where
    parseJSON = withObject "NewSplitsSpec" $ \o -> do
        title <- o .: "title"
        subtitle <- o .: "subtitle"
        game <- o .: "game"
        category <- o .: "category"
        segments <- o .: "segments"
        return NewSplitsSpec{..}

instance JSON.ToJSON GameSpec
instance JSON.ToJSON CategorySpec
instance JSON.ToJSON SegmentSpec
instance JSON.ToJSON NewSplitsSpec
