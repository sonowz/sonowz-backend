module Sonowz.Raytrace.Web.Websocket
  ( websocketHandler
  )
where

import Relude hiding (newEmptyMVar, takeMVar)
import Relude.Extra.Newtype (un)
import UnliftIO (MonadUnliftIO(..))
import UnliftIO.Async (async, waitAnyCancel)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (finally, bracket)

import qualified Network.WebSockets as WS
import qualified Database.PostgreSQL.Simple as PGS

import Sonowz.Raytrace.Core.Has (Has(..), MonadHas(..))
import Sonowz.Raytrace.Monad.MQueue (MonadMQueue(..), WithDb)
import Sonowz.Raytrace.Monad.MQueue.DaemonDB (enqueueDaemonDBNew)
import Sonowz.Raytrace.Monad.MQueue.ServantDB (withServantQueue)
import Sonowz.Raytrace.Monad.MQueue.Db.Types
  ( ServantId
  , ServantMessage
  , DaemonMessage
  , ServantOp(..)
  , DaemonOp(..)
  , MessageQueue(..)
  , emptyMessage
  )
import Sonowz.Raytrace.Monad.MQueueThread
  (ThreadHandler, HandlerResult(..), WithMQueues, runMQueueThread)
import Sonowz.Raytrace.Monad.Websocket (MonadWebsocket(..), WSMessage(..))
import Sonowz.Raytrace.RaytraceConfig (jsonToConfig, Config(..), ConfigResult(..))


websocketHandler
  :: ( MonadHas WS.Connection m
     , MonadHas PGS.Connection m
     , MonadMQueue DaemonMessage m
     , MonadWebsocket m
     , MonadUnliftIO m
     )
  => m ()
websocketHandler = flip finally sendCloseSignal $ do
  config <- getRunnerConfig
  bracket (enqueueRaytrace config) dequeueRaytrace forkWaitProgressThreads

-- Watch raytrace progress & receive ping from client
forkWaitProgressThreads
  :: (MonadHas WS.Connection m, WithDb m, MonadUnliftIO m, Monad m) => ServantId -> m ()
forkWaitProgressThreads servantId' = do
  env               <- makeRaytraceProgressEnv
  tRaytraceProgress <- async (withServantQueue servantId' env raytraceProgressThread)
  tPing             <- async pingThread
  waitAnyCancel [tRaytraceProgress, tPing] -- If any of two exits, close websocket
  pass

getRunnerConfig :: (MonadWebsocket m, MonadIO m) => m Config
getRunnerConfig = encodeUtf8 <$> receiveText >>= makeRunnerConfig where

  receiveText :: MonadWebsocket m => m Text
  receiveText = un <$> wrapTimeout 3 getWSMessage

  makeRunnerConfig :: MonadIO m => LByteString -> m Config
  makeRunnerConfig json = case jsonToConfig json of
    DecodeFail    errormsg -> exitFailure -- TODO: logging
    ConfigSuccess config   -> return config

enqueueRaytrace :: WithDb m => Config -> m ServantId
enqueueRaytrace config = enqueueDaemonDBNew (Enqueue config)

dequeueRaytrace :: MonadMQueue DaemonMessage m => ServantId -> m ()
dequeueRaytrace servantId' = enqueue dequeueMessage where
  dequeueMessage = emptyMessage { servantId = servantId', operation = Dequeue } :: DaemonMessage

instance MonadWebsocket m => MonadMQueue WSMessage m where
  enqueue = putWSMessage
  dequeue = Just <$> getWSMessage

data RaytraceProgressEnv = RaytraceProgressEnv
  { wsConn :: WS.Connection
  , pgConn :: PGS.Connection
  }
instance Has WS.Connection RaytraceProgressEnv where
  obtain = wsConn
instance Has PGS.Connection RaytraceProgressEnv where
  obtain = pgConn

makeRaytraceProgressEnv
  :: (MonadHas WS.Connection m, MonadHas PGS.Connection m, Monad m) => m RaytraceProgressEnv
makeRaytraceProgressEnv = do
  wsConn <- grab @WS.Connection
  pgConn <- grab @PGS.Connection
  return RaytraceProgressEnv { .. }

raytraceProgressThread :: (WithMQueues m ServantMessage WSMessage, MonadHas ServantId m) => m ()
raytraceProgressThread = do
  servantId' <- grab @ServantId
  runMQueueThread (raytraceProgressThread' servantId') where
  raytraceProgressThread' :: Monad m => ServantId -> ThreadHandler m ServantMessage WSMessage
  raytraceProgressThread' servantId' MessageQueue {..} = return (handle servantId' operation)
  handle :: ServantId -> ServantOp -> HandlerResult WSMessage
  handle _   Enqueued           = HContinue
  handle _   Dequeued           = HTerminate
  handle _   (RemainingQueue n) = hSend ("Job queued: " <> show n <> " jobs remaining")
  handle _   ProcessStarted     = hSend "Processing image..."
  handle id' ProcessFinished    = hSendTerminate ("Finished: " <> show id')
  handle _   ProcessFailed      = hSendTerminate "Finished: -1"
  hSend          = HSend . WSMessage
  hSendTerminate = HSendTerminate . WSMessage

pingThread :: (MonadHas WS.Connection m, MonadIO m) => m ()
pingThread = do
  conn <- grab @WS.Connection
  forever $ do
    liftIO $ WS.receive conn
    threadDelay (10 * 10 ^ 6)
