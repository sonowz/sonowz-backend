module Sonowz.Raytrace.Daemon.Raytrace
  ( forkRaytraceDaemon
  )
where

import Relude
import Data.Time.LocalTime (getZonedTime)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (async, cancel, waitAnyCancel, waitCatch)
import Turtle (ExitCode(ExitSuccess, ExitFailure))

import Sonowz.Raytrace.Core.Has (Has(..), MonadHas(..))
import Sonowz.Raytrace.Core.DB (DBConnPool)
import Sonowz.Raytrace.Daemon.Types
import Sonowz.Raytrace.Monad.MQueue (MonadMQueue(..))
import Sonowz.Raytrace.Monad.MQueue.Db.Types
  ( ServantId(..)
  , ServantMessage
  , ServantOp(..)
  , DaemonMessage
  , DaemonOp(..)
  , MessageQueue(..)
  , emptyMessage
  )
import Sonowz.Raytrace.Monad.MQueue.IORef (IORefQueue, removeIORef)
import Sonowz.Raytrace.Monad.MQueue.ServantDB (withServantQueue)
import Sonowz.Raytrace.Monad.MQueueThread
  (ThreadHandler, HandlerResult(..), WithMQueues, runMQueueThread)
import Sonowz.Raytrace.RaytraceConfig (Config(..))
import qualified Sonowz.Raytrace.Daemon.RunnerScript as Script


forkRaytraceDaemon
  :: ( MonadMQueue DaemonMessage m
     , MonadMQueue RunInfo m
     , MonadHas (IORefQueue RunInfo) m
     , MonadHas CurrentRunInfo m
     , MonadHas DBConnPool m
     , MonadUnliftIO m
     , MonadIO m
     )
  => m ()
forkRaytraceDaemon = do
  tRunnerControl <- async runnerControlThread
  tRunner        <- async runnerThread
  waitAnyCancel [tRunner, tRunnerControl]
  pass


runnerThread
  :: (WithMQueues m RunInfo Void, MonadHas CurrentRunInfo m, MonadHas DBConnPool m, MonadUnliftIO m)
  => m ()
runnerThread = runMQueueThread handle where

  handle
    :: (MonadHas CurrentRunInfo m, MonadHas DBConnPool m, MonadUnliftIO m)
    => ThreadHandler m RunInfo Void
  handle runInfo@(RunInfo servantId' _) = do
    writeRaytraceStart servantId'
    runnerProcess <- async $ runRaytraceScript runInfo
    setCurrentRunInfo runInfo runnerProcess
    writeQueueStatus
    processResult <- waitCatch runnerProcess
    writeRaytraceResult servantId' processResult
    return HContinue

  setCurrentRunInfo :: (MonadHas CurrentRunInfo m, MonadIO m) => RunInfo -> RunnerProcess -> m ()
  setCurrentRunInfo = curry $ \currentRunInfo -> do
    CurrentRunInfo ref <- grab @CurrentRunInfo
    modifyIORef' ref (const currentRunInfo)

  logRaytrace :: MonadIO m => ServantId -> Text -> m ()
  logRaytrace (ServantId servantId') msg = do
    time <- liftIO $ fmap show getZonedTime
    let header = time <> ": Job #" <> show servantId' :: Text
    putTextLn (header <> " " <> msg)
  writeRaytraceStart :: (MonadHas DBConnPool m, MonadUnliftIO m) => ServantId -> m ()
  writeRaytraceStart servantId' = do
    sendToServant servantId' ProcessStarted
    logRaytrace servantId' "started."
  writeRaytraceResult
    :: (MonadHas DBConnPool m, MonadUnliftIO m)
    => ServantId
    -> Either SomeException Script.ShellResult
    -> m ()
  writeRaytraceResult servantId' processResult = case processResult of
    Left _ -> do
      sendToServant servantId' ProcessFailed
      logRaytrace servantId' "aborted."
    Right (Script.ShellResult (ExitFailure _) out err) -> do
      sendToServant servantId' ProcessFailed
      logRaytrace servantId' "failed."
      putTextLn out
      putTextLn err
    Right (Script.ShellResult ExitSuccess _ _) -> do
      sendToServant servantId' ProcessFinished
      logRaytrace servantId' "finished."
  writeQueueStatus :: Monad m => m ()
  writeQueueStatus = pass -- TODO: send 'RemainingQueue' to servant


runnerControlThread
  :: ( WithMQueues m DaemonMessage RunInfo
     , MonadHas CurrentRunInfo m
     , MonadHas (IORefQueue RunInfo) m
     , MonadHas DBConnPool m
     , MonadUnliftIO m
     )
  => m ()
runnerControlThread = runMQueueThread runnerControlThread' where

  runnerControlThread'
    :: ( MonadHas CurrentRunInfo m
       , MonadHas (IORefQueue RunInfo) m
       , MonadHas DBConnPool m
       , MonadUnliftIO m
       )
    => ThreadHandler m DaemonMessage RunInfo
  runnerControlThread' MessageQueue {..} = handle servantId operation

  handle
    :: ( MonadHas CurrentRunInfo m
       , MonadHas (IORefQueue RunInfo) m
       , MonadHas DBConnPool m
       , MonadUnliftIO m
       , Monad m
       )
    => ServantId
    -> DaemonOp
    -> m (HandlerResult RunInfo)
  handle servantId' (Enqueue config) = do
    sendToServant servantId' Enqueued
    return $ HSend (RunInfo servantId' config)
  handle servantId' Dequeue = do
    removeFromQueue servantId'
    stopRunnerIfDequeued servantId'
    return HContinue

  removeFromQueue servantId' = removeIORef (\(RunInfo sid _) -> sid == servantId')

  stopRunnerIfDequeued :: (MonadHas CurrentRunInfo m, MonadIO m) => ServantId -> m ()
  stopRunnerIfDequeued servantId' = do
    CurrentRunInfo ref                <- grab @CurrentRunInfo
    (RunInfo runSid _, runnerProcess) <- readIORef ref
    if runSid == servantId' then liftIO (cancel runnerProcess) else pass


runRaytraceScript :: MonadIO m => RunInfo -> m Script.ShellResult
runRaytraceScript (RunInfo (ServantId servantId') (Config config)) = do
  -- TODO: include these in Env
  let raytracePath = "/home/sonowz/packages/raytrace"
  let outputPath = "/home/sonowz/data/www/graphics-demo/image/raytrace"
  let imageId      = servantId' :: Int
  liftIO $ Script.raytraceScript imageId config raytracePath outputPath

instance Has DBConnPool DBConnPool where
  obtain = id

sendToServant :: (MonadHas DBConnPool m, MonadUnliftIO m) => ServantId -> ServantOp -> m ()
sendToServant servantId operation = do
  pgConnPool <- grab @DBConnPool
  withServantQueue servantId pgConnPool (enqueue message) where
  message :: ServantMessage
  message = emptyMessage { servantId = servantId, operation = operation }
