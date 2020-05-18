module Sonowz.Raytrace.App.Web.Websocket
  ( websocketHandler
  )
where

import Control.Concurrent.Async (waitAnyCancel)
import Polysemy.Async (Async, asyncToIO)
import Polysemy.Resource (Resource, finally, bracket, resourceToIO)
import qualified Network.WebSockets as WS
import qualified Polysemy.Async as P

import Sonowz.Raytrace.Imports
import Sonowz.Raytrace.DB.Pool (DBConnPool, DBEffects)
import Sonowz.Raytrace.DB.Types
  ( ServantId(..)
  , ServantMessage
  , ServantOp(..)
  , DaemonMessage
  , DaemonOp(..)
  , Message(..)
  , emptyMessage
  )
import Sonowz.Raytrace.MessageQueue.Effect (MessageQueue, enqueue)
import Sonowz.Raytrace.MessageQueue.Effect.DB
  (runMQueueDBDaemon, runMQueueDBServant, enqueueDBDaemonNew)
import Sonowz.Raytrace.MessageQueue.Effect.Websocket (runMQueueWebsocket)
import Sonowz.Raytrace.MessageQueueThread.Effect
  (doStreamLoop, runMQueueStream, StreamHandler, StreamResult(..))
import Sonowz.Raytrace.RaytraceConfig (Config, ConfigResult(..), jsonToConfig)
import Sonowz.Raytrace.Time.Effect (Time, timeout, threadDelay, timeToIO)
import Sonowz.Raytrace.Websocket.Effect
  (Websocket, getWSMessage, sendCloseSignal, receiveAny, WSMessage(..), runWebsocketToIO)


websocketHandler :: DBConnPool -> WS.Connection -> IO ()
websocketHandler dbPool wsConn =
  websocketHandler' & runWebsocketToIO wsConn & resourceToIO & timeToIO & runM where
  websocketHandler' :: Members '[Websocket, Resource, Time, Embed IO] r => Sem r ()
  websocketHandler' = flip finally sendCloseSignal $ do
    config <- getRunnerConfig
    bracket (enqueueRaytrace config) dequeueRaytrace forkWaitProgressThreads
      & runMQueueDBDaemon
      & runReader dbPool
      & asyncToIO

-- Watch raytrace progress & receive ping from client
forkWaitProgressThreads
  :: (Members '[Websocket, Async, Resource, Time] r, Members DBEffects r) => ServantId -> Sem r ()
forkWaitProgressThreads servantId' = do
  tRaytraceProgress <- P.async (raytraceProgressThread & runMQueueDBServant & runReader servantId')
  tPing             <- P.async pingThread
  embed $ waitAnyCancel [tRaytraceProgress, tPing] -- If any of two exits, close websocket
  pass


getRunnerConfig :: Members '[Websocket, Time] r => Sem r Config
getRunnerConfig = encodeUtf8 <$> receiveText >>= makeRunnerConfig where

  receiveText :: Members '[Websocket, Time] r => Sem r Text
  receiveText = timeout (3 * 10 ^ 6) getWSMessage >>= \case
    Just (WSMessage text) -> return text
    Nothing               -> error "TODO: handle this with exception"

  makeRunnerConfig :: LByteString -> Sem r Config
  makeRunnerConfig json = case jsonToConfig json of
    DecodeFail    errormsg -> error "TODO: logging & exception"
    ConfigSuccess config   -> return config

enqueueRaytrace :: Members DBEffects r => Config -> Sem r ServantId
enqueueRaytrace config = enqueueDBDaemonNew (Enqueue config)

dequeueRaytrace :: Member (MessageQueue DaemonMessage) r => ServantId -> Sem r ()
dequeueRaytrace servantId' = enqueue dequeueMessage where
  dequeueMessage = emptyMessage { servantId = servantId', operation = Dequeue } :: DaemonMessage


-- https://github.com/lspitzner/brittany/issues/271
-- brittany-disable-next-binding
type RaytraceProgressEffects =
  [ Websocket
  , Time
  , MessageQueue ServantMessage
  , Reader ServantId
  ]

raytraceProgressThread :: Members RaytraceProgressEffects r => Sem r ()
raytraceProgressThread =
  doStreamLoop & runMQueueStream raytraceProgressThread' & subsume & runMQueueWebsocket where

  raytraceProgressThread'
    :: Members RaytraceProgressEffects r => StreamHandler r ServantMessage WSMessage
  raytraceProgressThread' Message {..} =
    ask >>= \servantId' -> return (handle servantId' operation)

  handle :: ServantId -> ServantOp -> StreamResult WSMessage
  handle _   Enqueued           = HContinue
  handle _   Dequeued           = HTerminate
  handle _   (RemainingQueue n) = hSend ("Job queued: " <> show n <> " jobs remaining")
  handle _   ProcessStarted     = hSend "Processing image..."
  handle id' ProcessFinished    = hSendTerminate ("Finished: " <> show (coerce id' :: Int))
  handle _   ProcessFailed      = hSendTerminate "Finished: -1"
  hSend          = HSend . WSMessage
  hSendTerminate = HSendTerminate . WSMessage

pingThread :: Members '[Websocket, Time] r => Sem r ()
pingThread = forever $ do
  receiveAny
  threadDelay (10 * 10 ^ 6)
