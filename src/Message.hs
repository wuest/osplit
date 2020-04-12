{-# LANGUAGE DeriveGeneric #-}

module Message where

import Prelude
import GHC.Generics  ( Generic )

import qualified Data.Aeson as JSON
import qualified Data.Text  as Text

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

data Command = TimeSyncInit TimeState
             | TimerControl SplitsCommand
    deriving ( Show, Generic )

type SplitSet = [SegmentTime]

data SegmentTime = SegmentTime { segment :: Int
                               , time    :: Int
                               } deriving ( Show, Generic )

data Response = Raw { respType :: Text.Text
                    , respData :: Text.Text
                    }
              | TimeSyncResponse TimeState
              | RemoteControl SplitsCommand
              deriving ( Show, Generic )

instance JSON.FromJSON SplitsCommand
instance JSON.ToJSON SplitsCommand
instance JSON.FromJSON TimeState
instance JSON.ToJSON TimeState
instance JSON.FromJSON SegmentTime
instance JSON.ToJSON SegmentTime
instance JSON.FromJSON Command
instance JSON.ToJSON Response
instance JSON.ToJSON Command
