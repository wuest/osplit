{-# LANGUAGE OverloadedStrings #-}

module Websockets ( initState
                  , app
                  , broadcast )
where

import Prelude
import Control.Monad ( forever )
import GHC.Generics  ( Generic )

import qualified Control.Concurrent             as Concurrent
import qualified Control.Concurrent.STM         as STM
import qualified Control.Exception              as Exception
import qualified Control.Monad                  as Monad
import qualified Data.List                      as List
import qualified Data.Maybe                     as Maybe
import qualified Data.Text                      as Text
import qualified Data.Aeson                     as JSON
import qualified Data.Time.Clock.POSIX          as Clock

import qualified Network.WebSockets             as WS

import qualified Data.Sqlite                    as DB
import qualified Database.Persist.Sqlite        as SQL
import qualified Data.Model                     as Model

import Database.Persist.Sqlite ( (==.), entityKey, entityVal )

import Control.Monad.IO.Class  ( liftIO )
import Control.Monad.Reader    ( runReaderT )
import Data.Text.Lazy.Encoding ( decodeUtf8 )
import Data.Text.Lazy          ( toStrict )

import qualified Message

type ClientId = Int
type Client   = (ClientId, WS.Connection)
type State    = [Client]

broadcast :: Concurrent.MVar State -> STM.TChan Text.Text -> IO loop
broadcast stateRef broadcastChan = do
    chan <- STM.atomically $ STM.dupTChan broadcastChan
    forever $ do
        msg <- STM.atomically $ STM.readTChan chan
        sendFrom (negate 1) stateRef msg

nextId :: State -> ClientId
nextId = Maybe.maybe 0 (1 +) . maxM . List.map fst

maxM :: Ord a => [a] -> Maybe a
maxM [] = Nothing
maxM xs = Just $ maximum xs

connectClient :: WS.Connection -> Concurrent.MVar State -> IO ClientId
connectClient conn stateRef = Concurrent.modifyMVar stateRef $ \state -> do
    let clientId = nextId state
    return ((clientId, conn) : state, clientId)

withoutClient :: ClientId -> State -> State
withoutClient clientId = List.filter ((/=) clientId . fst)

disconnectClient :: ClientId -> Concurrent.MVar State -> IO ()
disconnectClient clientId stateRef = Concurrent.modifyMVar_ stateRef $ \state ->
    return $ withoutClient clientId state

sendFrom :: ClientId -> Concurrent.MVar State -> Text.Text -> IO ()
sendFrom clientId stateRef msg = do
    clients <- Concurrent.readMVar stateRef
    let otherClients = withoutClient clientId clients
    Monad.forM_ otherClients $ \(_, conn) ->
        WS.sendTextData conn msg

sendTo :: ClientId -> Concurrent.MVar State -> Text.Text -> IO ()
sendTo clientId stateRef msg = do
    clients <- Concurrent.readMVar stateRef
    let client = List.filter ((==) clientId . fst) clients
    Monad.forM_ client $ \(_, conn) ->
        WS.sendTextData conn msg

sendJSON :: JSON.ToJSON a => WS.Connection -> a -> IO ()
sendJSON conn = (WS.sendTextData conn) . toStrict . decodeUtf8 . JSON.encode

startApp' :: DB.DBPool -> WS.Connection -> ClientId -> Concurrent.MVar State -> IO ()
startApp' db conn clientId stateRef = runReaderT (startApp conn clientId stateRef) db

startApp :: WS.Connection -> ClientId -> Concurrent.MVar State -> DB.DBPoolM ()
startApp conn clientId stateRef = do
    d <- liftIO $ WS.receiveData conn
    case (JSON.decode d :: Maybe Message.Command) of
      Nothing -> do
          liftIO $ sendJSON conn unsupportedResponse
          startApp conn clientId stateRef
      Just cmd -> do
          response <- processCommand cmd clientId stateRef
          liftIO $ sendJSON conn response
          startApp conn clientId stateRef

app :: DB.DBPool -> Concurrent.MVar State -> WS.PendingConnection -> IO ()
app db stateRef pendingConn = do
    conn <- WS.acceptRequest pendingConn
    clientId <- connectClient conn stateRef

    WS.withPingThread conn 10 (return ()) $
        Exception.finally
            (startApp' db conn clientId stateRef)
            (disconnectClient clientId stateRef)

initState :: IO (Concurrent.MVar State)
initState = Concurrent.newMVar []

processCommand :: Message.Command -> ClientId -> Concurrent.MVar State  -> DB.DBPoolM Message.Response
processCommand (Message.TimeSyncInit timeState) _ _ = do
    now <- liftIO $ round . (1000 *) <$> Clock.getPOSIXTime
    return $ Message.TimeSyncResponse $ Message.TimeState { Message.currentTime = now, Message.previousOffset = Just (now - Message.currentTime timeState) }

processCommand (Message.TimerControl (Message.RemoteStartSplit i)) clientId stateRef = do
    liftIO $ sendFrom clientId stateRef $ (toStrict . decodeUtf8 . JSON.encode) $ startSplit i
    return ackResponse
processCommand (Message.TimerControl (Message.RemoteUnsplit i)) clientId stateRef = do
    liftIO $ sendFrom clientId stateRef $ (toStrict . decodeUtf8 . JSON.encode) $ unsplit i
    return ackResponse
processCommand (Message.TimerControl (Message.RemoteSkip i)) clientId stateRef = do
    liftIO $ sendFrom clientId stateRef $ (toStrict . decodeUtf8 . JSON.encode) $ skip i
    return ackResponse
processCommand (Message.TimerControl (Message.RemoteStop i)) clientId stateRef = do
    liftIO $ sendFrom clientId stateRef $ (toStrict . decodeUtf8 . JSON.encode) $ stopSplits i
    return ackResponse
processCommand (Message.TimerControl (Message.RemoteReset)) clientId stateRef = do
    liftIO $ sendFrom clientId stateRef $ (toStrict . decodeUtf8 . JSON.encode) $ reset
    return ackResponse
processCommand (Message.TimerControl (Message.RemoteFinish s)) clientId stateRef = do
    liftIO $ sendFrom (negate 1) stateRef $ (toStrict . decodeUtf8 . JSON.encode) $ reset
    return ackResponse

processCommand (Message.Menu Message.MenuGames) clientId stateRef = do
    gameList >>= return
processCommand (Message.Menu (Message.MenuCategories i)) clientId stateRef = do
    categoryList i >>= return

processCommand _ _ _ = return unsupportedResponse

startSplit :: Int -> Message.Response
startSplit i = Message.RemoteControl (Message.RemoteStartSplit i)

unsplit :: Int -> Message.Response
unsplit i = Message.RemoteControl (Message.RemoteUnsplit i)

skip :: Int -> Message.Response
skip i = Message.RemoteControl (Message.RemoteSkip i)

stopSplits :: Int -> Message.Response
stopSplits i = Message.RemoteControl (Message.RemoteStop i)

reset :: Message.Response
reset = Message.RemoteControl Message.RemoteReset

ackResponse :: Message.Response
ackResponse = Message.Raw { Message.respType = "ack", Message.respData = "[]" }

unsupportedResponse :: Message.Response
unsupportedResponse = Message.Raw { Message.respType = "UnsupportedCommand", Message.respData = "[]" }

gameList :: DB.DBPoolM Message.Response
gameList = do
    games <- DB.run $ SQL.selectList [] [] :: DB.DBPoolM [SQL.Entity Model.Game]
    return $ Message.GameList $ fmap gameList' games

gameList' :: SQL.Entity Model.Game -> Message.Game
gameList' g = do
    let game = entityVal g
        gameId = fromIntegral . SQL.fromSqlKey . entityKey $ g
      in Message.Game gameId game

categoryList :: Int -> DB.DBPoolM Message.Response
categoryList game = do
    games <- DB.run $ SQL.selectList [ Model.CategoryGame ==. (SQL.toSqlKey $ fromIntegral game) ] []
    return $ Message.CategoryList $ fmap categoryList' games

categoryList' :: SQL.Entity Model.Category -> Message.Category
categoryList' c = do
    let cat = entityVal c
        cid = fromIntegral . SQL.fromSqlKey . entityKey $ c
      in Message.Category cid cat
