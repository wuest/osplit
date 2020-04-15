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

data SplitsCommand = RemoteStartSplit Int
                   | RemoteFinish SplitSet
                   | RemoteUnsplit Int
                   | RemoteSkip Int
                   | RemoteStop Int
                   | RemoteReset
    deriving ( Show, Generic )

data MenuCommand = MenuGames
                 | MenuCategories Int
    deriving ( Show, Generic )

data Command = TimeSyncInit TimeState
             | TimerControl SplitsCommand
             | Menu MenuCommand
    deriving ( Show, Generic )

type SplitSet = [SegmentTime]

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
              deriving ( Show, Generic )

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
