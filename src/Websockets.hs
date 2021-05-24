{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Websockets ( initState
                  , app
                  , broadcast )
where

import Prelude
import Control.Monad ( forever, mzero )
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
import qualified Data.ConfigStore as Store

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

processCommand (Message.NewClient) clientId stateRef = do
    let k = "splits.active_splits"
    vs <- Store.fetch_ k
    return $ case vs of
               []  -> ackResponse
               v:_ -> Message.ConfigStore k v

processCommand (Message.TimerControl (Message.RemoteStartSplit i)) clientId stateRef = do
    liftIO $ sendFrom clientId stateRef $ (toStrict . decodeUtf8 . JSON.encode) $ startSplit i
    return ackResponse
processCommand (Message.TimerControl Message.RemoteUnsplit) clientId stateRef = do
    liftIO $ sendFrom clientId stateRef $ (toStrict . decodeUtf8 . JSON.encode) unsplit
    return ackResponse
processCommand (Message.TimerControl Message.RemoteSkip) clientId stateRef = do
    liftIO $ sendFrom clientId stateRef $ (toStrict . decodeUtf8 . JSON.encode) skip
    return ackResponse
processCommand (Message.TimerControl Message.RemoteStop) clientId stateRef = do
    liftIO $ sendFrom clientId stateRef $ (toStrict . decodeUtf8 . JSON.encode) stopSplits
    return ackResponse
processCommand (Message.TimerControl (Message.RemoteReset s)) clientId stateRef = do
    _ <- saveRun False s
    return $ ackResponse
    newSplits <- loadSplits $ Message.runCategory s
    liftIO $ sendFrom clientId stateRef $ (toStrict . decodeUtf8 . JSON.encode) $ reset
    liftIO $ sendFrom (negate 1) stateRef $ (toStrict . decodeUtf8 . JSON.encode) $ newSplits
    return ackResponse
processCommand (Message.TimerControl (Message.RemoteFinish s)) clientId stateRef = do
    _ <- saveRun True s
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
    Store.set "splits.active_splits" c
    newSplits <- loadSplits c
    liftIO $ sendFrom clientId stateRef $ (toStrict . decodeUtf8 . JSON.encode) $ newSplits
    return newSplits

processCommand (Message.SetConfig k v) clientId stateRef = do
    Store.set k v
    return ackResponse

processCommand (Message.FetchConfig k) clientId stateRef = do
    vs <- Store.fetch_ k
    return $ case vs of
               []  -> ackResponse
               v:_ -> Message.ConfigStore k v

processCommand (Message.NewSplits (Message.NewSplitsSpec title subtitle game' category' segments)) clientId stateRef = do
    game <- createFetchGame game'
    category <- createCategory game category'
    mapM_ (createSegment category) segments
    let c = fromIntegral . SQL.fromSqlKey $ category
    Store.set "splits.active_splits" c
    newSplits <- loadSplits c
    liftIO $ sendFrom clientId stateRef $ (toStrict . decodeUtf8 . JSON.encode) $ newSplits
    return newSplits

processCommand _ _ _ = return unsupportedResponse

startSplit :: Int -> Message.Response
startSplit = Message.RemoteControl . Message.RemoteStartSplit

unsplit :: Message.Response
unsplit = Message.RemoteControl Message.RemoteUnsplit

skip :: Message.Response
skip = Message.RemoteControl Message.RemoteSkip

stopSplits :: Message.Response
stopSplits = Message.RemoteControl Message.RemoteStop

reset :: Message.Response
reset = Message.RemoteControl (Message.RemoteReset (Message.SplitSet (negate 1) [] 0 0 0))

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
    pbRun  <- DB.run $ SQL.selectList [ Model.AttemptCompleted ==. True, Model.AttemptCategory ==. (Model.segmentCategory segment) ] [ SQL.Asc Model.AttemptRealTime, SQL.LimitTo 1 ]
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
    segment <- DB.run $ SQL.selectList [ Model.SplitAttempt ==. (entityKey run), Model.SplitSegment ==. skey ] []
    return $ case null segment of
      True -> Nothing
      False -> Just . Model.splitElapsed . entityVal $ head segment

saveRun :: Bool -> Message.SplitSet -> DB.DBPoolM ()
saveRun finished (Message.SplitSet cat segments start end realTime) = do
    let c       = SQL.toSqlKey $ fromIntegral cat
        s       = millisToUTCTime start
        e       = millisToUTCTime end
        attempt = Model.Attempt c s False e False realTime finished -- TODO: BUG: Fix NTP syncing for this
    attemptID <- DB.run $ SQL.insert attempt
    mapM_ (saveSegment attemptID) segments
    return ()

saveSegment :: Model.Key Model.Attempt -> Message.SegmentTime -> DB.DBPoolM ()
saveSegment aid st =
    let segment = SQL.toSqlKey . fromIntegral $ Message.segment st
        split   = Model.Split aid segment (Message.time st)
    in DB.run $ SQL.insert_ split

millisToUTCTime :: Int -> Time.UTCTime
millisToUTCTime time = Clock.posixSecondsToUTCTime $ (fromIntegral time) / 1000

createFetchGame :: Message.GameSpec -> DB.DBPoolM (Model.Key Model.Game)
createFetchGame (Message.GameSpec name icon offset) = do
    c <- DB.run $ SQL.count [ Model.GameName ==. name ]
    if c < 1
    then
        DB.run $ SQL.insert $ Model.Game name icon (fromIntegral offset)
    else do
        gs <- DB.run $ SQL.selectList [ Model.GameName ==. name ] [] :: DB.DBPoolM [SQL.Entity Model.Game]
        case gs of
          g:_ -> return $ entityKey g
          [] -> mzero -- Only possible via race condition of check vs select

createCategory :: Model.Key Model.Game -> Message.CategorySpec -> DB.DBPoolM (Model.Key Model.Category)
createCategory g (Message.CategorySpec name offset) = DB.run $ SQL.insert $ Model.Category name g (fromIntegral offset)

createSegment :: Model.Key Model.Category -> Message.SegmentSpec -> DB.DBPoolM ()
createSegment cat (Message.SegmentSpec name icon) = DB.run $ SQL.insert_ $ Model.Segment cat name icon
