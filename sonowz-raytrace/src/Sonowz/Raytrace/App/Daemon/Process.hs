module Sonowz.Raytrace.App.Daemon.Process
  ( forkRaytraceDaemon,
  )
where

import Control.Concurrent.Async (async, asyncThreadId, cancel, waitAnyCatchCancel, waitCatch)
import Polysemy.Async (asyncToIOFinal)
import Polysemy.Async qualified as P
import Polysemy.Resource (resourceToIOFinal)
import Sonowz.Core.DB.Pool (DBConnPool, DBEffects)
import Sonowz.Core.MessageQueue.Effect (MessageQueue, enqueue)
import Sonowz.Core.MessageQueue.Effect.State (removeMQueueState, runMQueueState)
import Sonowz.Core.MessageQueue.Effect.Void (runMQueueVoid)
import Sonowz.Core.MessageQueueThread.Effect
  ( StreamHandler,
    StreamResult (..),
    doStreamLoop,
    runMQueueStream,
  )
import Sonowz.Core.Time.Effect (Time, timeToIOFinal)
import Sonowz.Raytrace.App.Daemon.RunnerScript qualified as Script
import Sonowz.Raytrace.App.Daemon.Types (CurrentRunInfo (..), RunInfo (..), RunnerProcess (..))
import Sonowz.Raytrace.DB.Types
  ( DaemonMessage,
    DaemonOp (..),
    Message (..),
    ServantId (..),
    ServantMessage,
    ServantOp (..),
    emptyMessage,
  )
import Sonowz.Raytrace.Imports
import Sonowz.Raytrace.MessageQueue.Effect.DB (runMQueueDBDaemon, runMQueueDBServant)
import Sonowz.Raytrace.RaytraceConfig (Config (..))
import System.Exit (ExitCode (ExitFailure, ExitSuccess))

forkRaytraceDaemon :: (HasCallStack) => DBConnPool -> IO ()
forkRaytraceDaemon pool = do
  runInfoQueue <- newTVarIO ([] :: [RunInfo])
  currentRunInfo <-
    newTVarIO
      (error "Attempt to access uninitialized CurrentRunInfo" :: CurrentRunInfo)
  doFork
    & runMQueueDBDaemon
    & runMQueueState
    & subsume @(AtomicState [RunInfo]) -- For 'runMQueueState'
    & runAtomicStateTVar runInfoQueue
    & runAtomicStateTVar currentRunInfo
    & runReader pool
    & embedToFinal
    & timeToIOFinal
    & asyncToIOFinal
    & resourceToIOFinal
    & stdEffToIOFinal
    & runFinal
  where
    doFork ::
      (Member P.Async r, Members RunnerEffects r, Members RunnerControlEffects r, HasCallStack) =>
      Sem r ()
    doFork = void
      $ P.async
      $ do
        logDebug "Forking 'runnerControlThread'.."
        tRunnerControl <- P.async runnerControlThread
        logDebug "Forking 'runnerThread'.."
        tRunner <- P.async runnerThread
        (aborted, _) <- liftIO $ waitAnyCatchCancel [tRunner, tRunnerControl]
        if ((==) `on` asyncThreadId) aborted tRunnerControl
          then logError "'runnerControlThread' was aborted."
          else logError "'runnerThread' was aborted."
        doFork

-- Actual Runner Thread --

type RunnerEffects =
  [ Time,
    MessageQueue RunInfo,
    AtomicState CurrentRunInfo
  ]
    <> DBEffects

runnerThread :: (Members RunnerEffects r) => Sem r ()
runnerThread = doStreamLoop & runMQueueStream handle & runMQueueVoid
  where
    handle :: (Members RunnerEffects r) => StreamHandler r RunInfo Void
    handle runInfo@(RunInfo servantId' _) = do
      writeRaytraceStart servantId'
      writeQueueStatus
      runnerProcess <- liftIO $ async $ runRaytraceScript runInfo
      setCurrentRunInfo runInfo (RunnerProcess runnerProcess)
      processResult <- liftIO $ waitCatch runnerProcess
      writeRaytraceResult servantId' processResult
      return HContinue

    setCurrentRunInfo ::
      (Member (AtomicState CurrentRunInfo) r) => RunInfo -> RunnerProcess -> Sem r ()
    setCurrentRunInfo = curry (atomicPut . CurrentRunInfo)

    writeRaytraceStart :: (Members DBEffects r, HasCallStack) => ServantId -> Sem r ()
    writeRaytraceStart servantId' = do
      sendToServant servantId' ProcessStarted
      logInfo $ jobHeader servantId' <> "started."

    writeRaytraceResult ::
      (Members DBEffects r, HasCallStack) =>
      ServantId ->
      Either SomeException Script.ShellResult ->
      Sem r ()
    writeRaytraceResult servantId' processResult = case processResult of
      Left _ -> do
        sendToServant servantId' ProcessFailed
        logInfo $ jobHeader servantId' <> "aborted."
      Right (Script.ShellResult (ExitFailure _) out err) -> do
        sendToServant servantId' ProcessFailed
        logInfo $ jobHeader servantId' <> "failed."
        putTextLn out
        putTextLn err
      Right (Script.ShellResult ExitSuccess _ _) -> do
        sendToServant servantId' ProcessFinished
        logInfo $ jobHeader servantId' <> "finished."

    writeQueueStatus :: Sem r ()
    writeQueueStatus = pass -- TODO: send 'RemainingQueue' to servant

-- Runner Control Thread --

type RunnerControlEffects =
  [ Time,
    MessageQueue DaemonMessage,
    MessageQueue RunInfo,
    AtomicState [RunInfo],
    AtomicState CurrentRunInfo
  ]
    <> DBEffects

runnerControlThread :: (Members RunnerControlEffects r) => Sem r ()
runnerControlThread = doStreamLoop & runMQueueStream runnerControlThread'
  where
    runnerControlThread' :: (Members RunnerControlEffects r) => StreamHandler r DaemonMessage RunInfo
    runnerControlThread' Message {..} = handle servantId operation

    handle ::
      (Members RunnerControlEffects r, HasCallStack) =>
      ServantId ->
      DaemonOp ->
      Sem r (StreamResult RunInfo)
    handle servantId' (Enqueue config) = do
      logInfo $ jobHeader servantId' <> "queued."
      sendToServant servantId' Enqueued
      return $ HSend (RunInfo servantId' config)
    handle servantId' Dequeue = do
      removeFromQueue servantId'
      stopRunnerIfDequeued servantId'
      return HContinue

    removeFromQueue :: (Member (AtomicState [RunInfo]) r) => ServantId -> Sem r Bool
    removeFromQueue servantId' = removeMQueueState (\(RunInfo sid _) -> sid == servantId')

    stopRunnerIfDequeued ::
      (Members '[AtomicState CurrentRunInfo, Embed IO] r, Members StdEff r) =>
      ServantId ->
      Sem r ()
    stopRunnerIfDequeued servantId' = do
      CurrentRunInfo (RunInfo runSid _, RunnerProcess runnerProcess) <- atomicGet
      if runSid == servantId'
        then logDebug ("Aborting " <> jobHeader servantId') >> liftIO (cancel runnerProcess)
        else pass

-- Utility Functions --

jobHeader :: ServantId -> Text
jobHeader (ServantId servantId') = "Job #" <> show servantId' <> " "

sendToServant :: (Members DBEffects r) => ServantId -> ServantOp -> Sem r ()
sendToServant servantId operation = do
  let message :: ServantMessage = emptyMessage {servantId = servantId, operation = operation}
  enqueue message & runMQueueDBServant & runReader servantId

runRaytraceScript :: RunInfo -> IO Script.ShellResult
runRaytraceScript (RunInfo (ServantId servantId') (Config config)) = do
  -- TODO: include these in Env
  let raytracePath = "/home/sonowz/packages/raytrace"
  let outputPath = "/home/sonowz/data/www/graphics-demo/image/raytrace"
  let imageId = servantId' :: Int
  Script.raytraceScript imageId config raytracePath outputPath
