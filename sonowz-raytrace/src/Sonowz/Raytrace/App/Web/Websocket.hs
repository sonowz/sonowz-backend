module Sonowz.Raytrace.App.Web.Websocket
  ( websocketHandler,
  )
where

import Control.Concurrent.Async (asyncThreadId, waitAnyCatchCancel)
import Network.WebSockets qualified as WS
import Polysemy.Async (Async, asyncToIOFinal)
import Polysemy.Async qualified as P
import Polysemy.Resource (Resource, bracket, finally, resourceToIOFinal)
import Sonowz.Core.DB.Pool (DBConnPool, DBEffects)
import Sonowz.Core.MessageQueue.Effect (MessageQueue, enqueue)
import Sonowz.Core.MessageQueueThread.Effect
  ( StreamHandler,
    StreamResult (..),
    doStreamLoop,
    runMQueueStream,
  )
import Sonowz.Core.Time.Effect (Time, threadDelay, timeToIO, timeout)
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
import Sonowz.Raytrace.MessageQueue.Effect.DB
  ( enqueueDBDaemonNew,
    runMQueueDBDaemon,
    runMQueueDBServant,
  )
import Sonowz.Raytrace.MessageQueue.Effect.Websocket (runMQueueWebsocket)
import Sonowz.Raytrace.RaytraceConfig (Config, ConfigResult (..), jsonToConfig)
import Sonowz.Raytrace.Websocket.Effect
  ( WSMessage (..),
    Websocket,
    getWSMessage,
    receiveAny,
    runWebsocketToIO,
    sendCloseSignal,
  )

data WSException = WSException Text
  deriving (Show, Exception)

websocketHandler :: DBConnPool -> WS.Connection -> IO ()
websocketHandler dbPool wsConn =
  websocketHandler'
    & runWebsocketToIO wsConn
    & runMQueueDBDaemon
    & runReader dbPool
    & stdEffToIO
    & timeToIO
    & asyncToIOFinal
    & resourceToIOFinal
    & embedToFinal
    & runFinal
  where
    websocketHandler' ::
      ( Members '[Websocket, Async, Resource, Time, MessageQueue DaemonMessage] r,
        Members DBEffects r
      ) =>
      Sem r ()
    websocketHandler' = flip finally sendCloseSignal $ do
      logInfo "Websocket connection established."
      config <- getRunnerConfig
      bracket (enqueueRaytrace config) dequeueRaytrace forkWaitProgressThreads

-- Watch raytrace progress & receive ping from client
forkWaitProgressThreads ::
  (Members '[Websocket, Async, Resource, Time] r, Members DBEffects r, HasCallStack) =>
  ServantId ->
  Sem r ()
forkWaitProgressThreads servantId' = do
  logDebug "Forking 'raytraceProgressThread'.."
  tRaytraceProgress <- P.async (raytraceProgressThread & runMQueueDBServant & runReader servantId')
  logDebug "Forking 'pingThread'.."
  tPing <- P.async pingThread
  -- If any of two exits, close websocket
  (aborted, _) <- liftIO $ waitAnyCatchCancel [tRaytraceProgress, tPing]
  if ((==) `on` asyncThreadId) aborted tRaytraceProgress
    then logInfo "'raytraceProgressThread' finished, or was aborted."
    else logError "'pingThread' was aborted."
  pass

getRunnerConfig :: (Members '[Websocket, Time] r, Members StdEff r) => Sem r Config
getRunnerConfig = receiveText >>= makeRunnerConfig . encodeUtf8
  where
    receiveText :: (Members '[Websocket, Time] r, Members StdEff r) => Sem r Text
    receiveText =
      timeout (3 * 10 ^ 6) getWSMessage >>= \case
        Just (WSMessage text) -> return text
        Nothing -> throw' (WSException "Failed to receive config from client")

    makeRunnerConfig :: Members StdEff r => LByteString -> Sem r Config
    makeRunnerConfig json = case jsonToConfig json of
      DecodeFail errormsg ->
        let exception = WSException $ "Failed to parse config: " <> errormsg
         in logException exception >> throw' exception
      ConfigSuccess config -> return config

enqueueRaytrace :: (Members DBEffects r, HasCallStack) => Config -> Sem r ServantId
enqueueRaytrace config = do
  logDebug "Sending raytrace request to daemon.."
  enqueueDBDaemonNew (Enqueue config)

dequeueRaytrace ::
  (Member (MessageQueue DaemonMessage) r, Members StdEff r, HasCallStack) =>
  ServantId ->
  Sem r ()
dequeueRaytrace servantId' = do
  logDebug "Sending raytace cancel request to daemon.."
  enqueue dequeueMessage
  where
    dequeueMessage = emptyMessage {servantId = servantId', operation = Dequeue} :: DaemonMessage

type RaytraceProgressEffects =
  [ Websocket,
    Time,
    MessageQueue ServantMessage,
    Reader ServantId
  ]
    <> StdEff

raytraceProgressThread :: Members RaytraceProgressEffects r => Sem r ()
raytraceProgressThread =
  doStreamLoop & runMQueueStream raytraceProgressThread' & runMQueueWebsocket
  where
    raytraceProgressThread' ::
      Members RaytraceProgressEffects r => StreamHandler r ServantMessage WSMessage
    raytraceProgressThread' Message {..} =
      ask >>= \servantId' -> return (handle servantId' operation)

    handle :: ServantId -> ServantOp -> StreamResult WSMessage
    handle _ Enqueued = HContinue
    handle _ Dequeued = HTerminate
    handle _ (RemainingQueue n) = hSend ("Job queued: " <> show n <> " jobs remaining")
    handle _ ProcessStarted = hSend "Processing image..."
    handle id' ProcessFinished = hSendTerminate ("Finished: " <> show (coerce id' :: Int))
    handle _ ProcessFailed = hSendTerminate "Finished: -1"
    hSend = HSend . WSMessage
    hSendTerminate = HSendTerminate . WSMessage

pingThread :: (Members '[Websocket, Time] r, Members StdEff r) => Sem r ()
pingThread = forever $ do
  timeout (10 * (10 ^ 6)) receiveAny
  logDebug "Ping received from client."
  threadDelay (10 ^ 6)
