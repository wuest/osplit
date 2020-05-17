{-# LANGUAGE OverloadedStrings          #-}

module Data.ConfigStore where

import Prelude

import GHC.Generics            ( Generic )
import Data.Aeson              ( FromJSON, ToJSON, decode, encode )
import Database.Persist.Sqlite ( (==.), (=.), entityKey, entityVal )

import qualified Data.Sqlite             as DB
import qualified Database.Persist.Sqlite as SQL
import qualified Data.Model              as Model

import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text ( encodeUtf8, decodeUtf8 )
import qualified Data.ByteString.Lazy as BS   ( fromStrict, toStrict )

fetch :: FromJSON j => Text.Text -> DB.DBPoolM ([Maybe j])
fetch key = do
    object <- DB.run $ SQL.selectList [ Model.ConfigStoreKey ==. key ] []
    return $ fmap (decode . BS.fromStrict . Text.encodeUtf8 . Model.configStoreJsonValue . entityVal) object

fetch_ :: Text.Text -> DB.DBPoolM ([Text.Text])
fetch_ key = do
    object <- DB.run $ SQL.selectList [ Model.ConfigStoreKey ==. key ] []
    return $ fmap (Model.configStoreJsonValue . entityVal) object

set :: ToJSON j => Text.Text -> j -> DB.DBPoolM ()
set key val = do
    let val' = Text.decodeUtf8 . BS.toStrict . encode $ val
    exist <- DB.run $ SQL.count [ Model.ConfigStoreKey ==. key ]
    if exist > 0
       then do
           DB.run $ SQL.updateWhere [ Model.ConfigStoreKey ==. key ] [ Model.ConfigStoreJsonValue =. val' ]
           return ()
       else do
           let newKV = Model.ConfigStore key val'
           DB.run $ SQL.insert_ newKV
           return ()
