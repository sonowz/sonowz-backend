module Sonowz.Raytrace.Daemon.Raytrace
  ( forkRaytraceDaemon
  )
where

import Relude
import Data.Time.LocalTime (getZonedTime)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (Async, async, cancel, waitAnyCancel, waitCatch)
import Turtle (ExitCode(ExitSuccess, ExitFailure))
import qualified Database.PostgreSQL.Simple as PGS

import Sonowz.Raytrace.Core.Has (Has(..), MonadHas(..))
import Sonowz.Raytrace.Monad.MQueue (MonadMQueue(..))
import Sonowz.Raytrace.Monad.MQueue.Db.Types
  (ServantId(..), ServantMessage, ServantOp(..), DaemonMessage, DaemonOp(..), MessageQueue(..), emptyMessage)
import Sonowz.Raytrace.Monad.MQueue.RunInfoIORef (RunInfo(..), RunInfoQueue(..), remove)
import Sonowz.Raytrace.Monad.MQueue.ServantDB (withServantQueue)
import Sonowz.Raytrace.Monad.MQueueThread
  (ThreadHandler, HandlerResult(..), WithMQueues, runMQueueThread)
import Sonowz.Raytrace.RaytraceConfig (Config(..))
import qualified Sonowz.Raytrace.Daemon.RunnerScript as Script



type RunnerProcess = Async Script.ShellResult
newtype CurrentRunInfo = CurrentRunInfo (IORef (RunInfo, RunnerProcess))
type RunT m = StateT [RunInfo] m

forkRaytraceDaemon :: (MonadMQueue DaemonMessage m, MonadHas PGS.Connection m, MonadUnliftIO m, MonadIO m) => m ()
forkRaytraceDaemon = void $ flip runStateT [] $ do -- Stack with RunInfo queue
  tRunnerControl <- async runnerControlThread
  tRunner <- async runnerThread
  waitAnyCancel [tRunner, tRunnerControl]


runnerThread :: (WithMQueues m RunInfo Void, MonadHas CurrentRunInfo m, MonadHas PGS.Connection m, MonadUnliftIO m) => m ()
runnerThread = runMQueueThread handle where

  handle :: (MonadHas CurrentRunInfo m, MonadHas PGS.Connection m, MonadUnliftIO m) => ThreadHandler m RunInfo Void
  handle runInfo@(RunInfo servantId' _) = do
    runnerProcess <- async $ runRaytraceScript runInfo
    setCurrentRunInfo runInfo runnerProcess
    writeRaytraceStart servantId'
    writeQueueStatus
    processResult <- waitCatch runnerProcess
    writeRaytraceResult servantId' processResult
    return HContinue 
    
  setCurrentRunInfo :: (MonadHas CurrentRunInfo m, MonadIO m) => RunInfo -> RunnerProcess -> m ()
  setCurrentRunInfo = curry $ \currentRunInfo -> do
    CurrentRunInfo ref <- grab @CurrentRunInfo
    modifyIORef' ref (const currentRunInfo)

  logRaytrace :: MonadIO m => ServantId -> Text -> m ()
  logRaytrace servantId' msg = do
    time <- liftIO $ fmap show getZonedTime
    let header = time <> ": Job #" <> show servantId' :: Text
    putTextLn (header <> " " <> msg)
  writeRaytraceStart :: (MonadHas PGS.Connection m, MonadIO m)=> ServantId -> m ()
  writeRaytraceStart servantId' = do
    sendToServant servantId' ProcessStarted
    logRaytrace servantId' "started."
  writeRaytraceResult :: (MonadHas PGS.Connection m, MonadIO m) => ServantId -> Either SomeException Script.ShellResult -> m ()
  writeRaytraceResult servantId' processResult =
    case processResult of
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
      

runnerControlThread :: (WithMQueues (RunT m) DaemonMessage RunInfo, MonadHas CurrentRunInfo (RunT m), MonadHas PGS.Connection (RunT m), MonadIO m) => RunT m ()
runnerControlThread = runMQueueThread runnerControlThread' where

  runnerControlThread' :: (MonadHas CurrentRunInfo (RunT m), MonadHas PGS.Connection (RunT m), MonadIO m) => ThreadHandler (RunT m) DaemonMessage RunInfo
  runnerControlThread' MessageQueue {..} = handle servantId operation

  handle :: (MonadHas CurrentRunInfo (RunT m), MonadHas PGS.Connection (RunT m), MonadIO m, Monad m) => ServantId -> DaemonOp -> RunT m (HandlerResult RunInfo)
  handle servantId' (Enqueue config) = do
    sendToServant servantId' Enqueued
    return $ HSend (RunInfo servantId' config)
  handle servantId' Dequeue = do
    removeFromQueue servantId'
    stopRunnerIfDequeued servantId'
    return HContinue

  removeFromQueue servantId' = remove (\(RunInfo sid _) -> sid == servantId')

  stopRunnerIfDequeued :: (MonadHas CurrentRunInfo m, MonadIO m) => ServantId -> m ()
  stopRunnerIfDequeued servantId' = do
    CurrentRunInfo ref <- grab @CurrentRunInfo
    (RunInfo runSid _, runnerProcess) <- readIORef ref
    if runSid == servantId' then liftIO (cancel runnerProcess) else pass


runRaytraceScript :: MonadIO m => RunInfo -> m Script.ShellResult
runRaytraceScript (RunInfo (ServantId servantId') (Config config)) = do
  -- TODO: include these in Env
  let raytracePath = "/home/sonowz/packages/raytrace"
  let outputPath   = "/home/sonowz/data/www/graphics-demo/image/raytrace"
  let imageId = servantId' :: Int
  liftIO $ Script.raytraceScript imageId config raytracePath outputPath

instance Has PGS.Connection PGS.Connection where obtain = id

sendToServant :: (MonadHas PGS.Connection m, MonadIO m) => ServantId -> ServantOp -> m ()
sendToServant servantId operation = do
  pgConn <- grab @PGS.Connection
  withServantQueue servantId pgConn (enqueue message) where
    message :: ServantMessage
    message = emptyMessage
      { servantId = servantId
      , operation = operation
      }
