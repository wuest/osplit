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

import Control.Monad.IO.Class  ( liftIO )
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

sendTo :: WS.Connection -> Text.Text -> IO ()
sendTo = WS.sendTextData

sendJSON :: JSON.ToJSON a => WS.Connection -> a -> IO ()
sendJSON conn = (sendTo conn) . toStrict . decodeUtf8 . JSON.encode

startApp :: WS.Connection -> ClientId -> Concurrent.MVar State -> IO ()
startApp conn clientId stateRef = Monad.forever $ do
    d <- liftIO $ WS.receiveData conn
    case (JSON.decode d :: Maybe Message.Command) of
      Nothing -> do
          liftIO $ sendJSON conn unsupportedResponse
      Just cmd -> do
          response <- processCommand cmd clientId stateRef
          liftIO $ sendJSON conn response

app :: Concurrent.MVar State -> WS.PendingConnection -> IO ()
app stateRef pendingConn = do
    conn <- WS.acceptRequest pendingConn
    clientId <- connectClient conn stateRef
    WS.withPingThread conn 10 (return ()) $
        Exception.finally
            (startApp conn clientId stateRef)
            (disconnectClient clientId stateRef)

initState :: IO (Concurrent.MVar State)
initState = Concurrent.newMVar []

processCommand :: Message.Command -> ClientId -> Concurrent.MVar State  -> IO Message.Response
processCommand (Message.TimeSyncInit timeState) _ _ = do
    now <- round . (1000 *) <$> Clock.getPOSIXTime
    return $ Message.TimeSyncResponse $ Message.TimeState { Message.currentTime = now, Message.previousOffset = Just (now - Message.currentTime timeState) }
processCommand (Message.TimerControl (Message.RemoteStartSplit i)) clientId stateRef = do
    sendFrom clientId stateRef $ (toStrict . decodeUtf8 . JSON.encode) $ startSplit i
    return ackResponse
processCommand (Message.TimerControl (Message.RemoteUnsplit i)) clientId stateRef = do
    sendFrom clientId stateRef $ (toStrict . decodeUtf8 . JSON.encode) $ unsplit i
    return ackResponse
processCommand (Message.TimerControl (Message.RemoteStop i)) clientId stateRef = do
    sendFrom clientId stateRef $ (toStrict . decodeUtf8 . JSON.encode) $ stopSplits i
    return ackResponse
processCommand (Message.TimerControl (Message.RemoteReset i)) clientId stateRef = do
    sendFrom clientId stateRef $ (toStrict . decodeUtf8 . JSON.encode) $ reset i

    return ackResponse
processCommand _ _ _ = return unsupportedResponse

startSplit :: Int -> Message.Response
startSplit i = Message.RemoteControl (Message.RemoteStartSplit i)

unsplit :: Int -> Message.Response
unsplit i = Message.RemoteControl (Message.RemoteUnsplit i)

stopSplits :: Int -> Message.Response
stopSplits i = Message.RemoteControl (Message.RemoteStop i)

reset :: Int -> Message.Response
reset i = Message.RemoteControl (Message.RemoteReset i)

ackResponse :: Message.Response
ackResponse = Message.Raw { Message.respType = "ack", Message.respData = "[]" }

unsupportedResponse :: Message.Response
unsupportedResponse = Message.Raw { Message.respType = "UnsupportedCommand", Message.respData = "[]" }
