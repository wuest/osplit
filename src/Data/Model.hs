{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Data.Model where

import Prelude

import Data.Text                   ( Text )
import Data.Time                   ( UTCTime )
import GHC.Generics                ( Generic )
import Data.Aeson                  ( FromJSON, ToJSON )

import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Game
    name          Text unique
    icon          Text Maybe
    defaultOffset Int
    deriving Show Generic

Category
    name   Text
    game   GameId
    offset Int
    deriving Show Generic

MultiCategory
    name Text
    deriving Show Generic

Segment
    category CategoryId
    name     Text
    icon     Text Maybe
    deriving Show Generic

MultiCategoryEntry
    parent MultiCategoryId
    category CategoryId
    deriving Show Generic

Attempt
    category    CategoryId
    startTime   UTCTime
    startSynced Bool
    endTime     UTCTime
    endSynced   Bool
    realTime    Int
    completed   Bool
    deriving Show Generic

MultiCategoryAttempt
    multicategory MultiCategoryId
    startTime     UTCTime
    startSynced   Bool
    endTime       UTCTime
    endSynced     Bool
    realTime      Int
    completed     Bool
    deriving Show Generic

Split
    attempt AttemptId
    segment SegmentId
    elapsed Int
    deriving Show Generic

ConfigStore
    key       Text unique
    jsonValue Text
    deriving Show Generic
|]

instance FromJSON Game
instance ToJSON Game

instance FromJSON Category
instance ToJSON Category

instance FromJSON Segment
instance ToJSON Segment

instance FromJSON Attempt
instance ToJSON Attempt

instance FromJSON Split
instance ToJSON Split

instance FromJSON ConfigStore
instance ToJSON ConfigStore
