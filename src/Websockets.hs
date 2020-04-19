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
import qualified Data.Time                      as Time

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
    _ <- saveRun s
    newSplits <- loadSplits $ Message.runCategory s
    liftIO $ sendFrom (negate 1) stateRef $ (toStrict . decodeUtf8 . JSON.encode) $ reset
    liftIO $ sendFrom (negate 1) stateRef $ (toStrict . decodeUtf8 . JSON.encode) $ newSplits
    return ackResponse

processCommand (Message.Menu Message.MenuCloseSplits) clientId stateRef = do
    liftIO $ sendFrom clientId stateRef $ (toStrict . decodeUtf8 . JSON.encode) $ closeSplits
    return ackResponse
processCommand (Message.Menu Message.MenuGames) _ stateRef = gameList
processCommand (Message.Menu (Message.MenuCategories i)) clientId stateRef = categoryList i

processCommand (Message.Menu (Message.MenuLoadSplits c)) clientId stateRef = do
    newSplits <- loadSplits c
    liftIO $ sendFrom clientId stateRef $ (toStrict . decodeUtf8 . JSON.encode) $ newSplits
    return newSplits

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

closeSplits :: Message.Response
closeSplits = Message.CloseSplits

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

loadSplits :: Int -> DB.DBPoolM Message.Response
loadSplits cid = do
    category <- DB.run $ SQL.selectList [ Model.CategoryId ==. (SQL.toSqlKey $ fromIntegral cid) ] []
    loaded <- loadSplits' category
    return $ Message.SplitsRefresh loaded

loadSplits' :: [SQL.Entity Model.Category] -> DB.DBPoolM (Maybe Message.LoadedSplits)
loadSplits' [] = return Nothing
loadSplits' (c:_) = do
    let cat  = entityVal c
        cid  = fromIntegral . SQL.fromSqlKey . entityKey $ c
        game = Model.categoryGame cat
    games <- DB.run $ SQL.selectList [ Model.GameId ==. game ] []
    loaded <- loadSplits'' games cid cat
    return loaded

loadSplits'' :: [SQL.Entity Model.Game] -> Int -> Model.Category -> DB.DBPoolM (Maybe Message.LoadedSplits)
loadSplits'' [] _ _ = return Nothing
loadSplits'' (g:_) cid cat = do
    let game = entityVal g
        gid  = fromIntegral . SQL.fromSqlKey . entityKey $ g
    segments <- DB.run $ SQL.selectList [ Model.SegmentCategory ==. (SQL.toSqlKey $ fromIntegral cid) ] []
    segments' <- mapM segmentData segments
    return $ Just $ Message.LoadedSplits gid game cid cat segments'

-- This is N+1 as heck and it's gross and bad and I hate it, but I want it
-- working before I worry about making it not awful.
-- TODO: Fix N+1.  N+1 is not good for your soul.
segmentData :: SQL.Entity Model.Segment -> DB.DBPoolM Message.SegmentData
segmentData s = do
    let segment = entityVal s
        skey = entityKey s
        sid = fromIntegral . SQL.fromSqlKey $ skey
        name = Model.segmentName segment
        icon = Model.segmentIcon segment
    pbRun  <- DB.run $ SQL.selectList [ Model.AttemptCompleted ==. True ] [ SQL.Asc Model.AttemptRealTime, SQL.LimitTo 1 ]
    pbSegment <- segmentDataPB pbRun skey
    record <- DB.run $ SQL.selectList [ Model.SplitSegment ==. skey ] [ SQL.Asc Model.SplitElapsed ]
    let gold  = fmap (Model.splitElapsed . entityVal) (Maybe.listToMaybe record)
        worst = fmap (Model.splitElapsed . entityVal) (Maybe.listToMaybe . List.reverse $ record)
        avg   = case fmap (Model.splitElapsed . entityVal) record of
                  []     -> Nothing
                  (x:xs) -> Just $ (List.foldl' (+) x xs) `div` (1 + (length xs))
    return $ Message.SegmentData sid name icon pbSegment gold avg worst

segmentDataPB [] _ = return Nothing
segmentDataPB (run:_) skey = do
    segment <- DB.run $ SQL.selectList [ Model.SplitAttempt ==. (entityKey run) ] []
    return $ case null segment of
      True -> Nothing
      False -> Just . Model.splitElapsed . entityVal $ head segment

saveRun :: Message.SplitSet -> DB.DBPoolM ()
saveRun splitSet = do
    let c       = SQL.toSqlKey . fromIntegral $ Message.runCategory splitSet
        s       = millisToUTCTime . Message.startTime $ splitSet
        e       = millisToUTCTime . Message.endTime $ splitSet
        r       = Message.realTime splitSet
        attempt = Model.Attempt c s False e False r True -- TODO: BUG: Fix NTP syncing for this
    attemptID <- DB.run $ SQL.insert attempt
    mapM_ (saveSegment attemptID) (Message.segments splitSet)
    return ()

saveSegment :: Model.Key Model.Attempt -> Message.SegmentTime -> DB.DBPoolM ()
saveSegment aid st =
    let segment = SQL.toSqlKey . fromIntegral $ Message.segment st
        split   = Model.Split aid segment (Message.time st)
    in DB.run $ SQL.insert_ split

millisToUTCTime :: Int -> Time.UTCTime
millisToUTCTime time = Clock.posixSecondsToUTCTime $ (fromIntegral time) / 1000
