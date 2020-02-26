module Sonowz.Raytrace.Web.Websocket
  ( wsServer
  , initWSData
  , WSData(WSData)
  , WSMessage(..)
  )
where

import           Relude                  hiding ( newEmptyMVar
                                                , takeMVar
                                                )
import           Control.Monad.IO.Unlift
import           Servant
import           UnliftIO.Chan
import           UnliftIO.Concurrent
import           UnliftIO.Exception
import           UnliftIO.Timeout

import qualified Network.WebSockets            as WS
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as LBC

import qualified Sonowz.Raytrace.Types.API     as API
import qualified Sonowz.Raytrace.MessageQueue  as MessageQueue
import qualified Sonowz.Raytrace.RTConfigCreator
                                               as RTConfigCreator

-- IPC object between daemon and websocket process
data WSData = WSData (MessageQueue.DBConnection, Chan (MVar WSMessage))

data WSMessage =
    MessageId Int       -- message created (with id)
  | RemainingQueue Int  -- number of request in front of this
  | ProcessStarted      -- raytrace process has started
  | ProcessFinished Int -- raytrace process sucessfully created image (with id)
  | ProcessFailed       -- raytrace process failed during execution

data WSException = WSException String deriving Show
instance Exception WSException

instance MonadUnliftIO Servant.Handler where
  withRunInIO
    :: ((forall a . Servant.Handler a -> IO a) -> IO b) -> Servant.Handler b
  withRunInIO f = liftIO $ f (\h -> runHandler h >>= either throwIO return)

initWSData :: IO WSData
initWSData = do
  conn <- MessageQueue._init
  chan <- newChan
  return $ WSData (conn, chan)

wsServer :: WSData -> Server API.RaytraceWsAPI
wsServer wsdata = runWebsocket where
  runWebsocket :: MonadUnliftIO io => WS.Connection -> io ()
  runWebsocket conn = do
    --catch (do
    params            <- timeoutWrapper 3 (getParameters conn)
    config            <- makeConfig params
    (messageMVar, id) <- registerEvent wsdata config
    _ <- forkIO (catchConnError wsdata id $ doProxy conn messageMVar)
    catchConnError wsdata id $ ping conn
    return ()--) (\(WSException err) -> send conn (show err))

-- Receive ping from client
ping :: MonadIO io => WS.Connection -> io ()
ping conn = liftIO $ forever $ do
  WS.receive conn
  threadDelay (10 * 10 ^ 6)

---- Minor functions ----

-- Connection error wrapper & handler
catchConnError :: (MonadUnliftIO io) => WSData -> Int -> io a -> io a
catchConnError wsdata id work = catch work (handler wsdata id) where
  handler :: (MonadUnliftIO io) => WSData -> Int -> WS.ConnectionException -> io a
  handler wsdata id e =
    unregisterEvent wsdata id >> throwIO (WSException "client closed")

-- Timeout setter wrapper
timeoutWrapper :: MonadUnliftIO io => Int -> io a -> io a
timeoutWrapper time obj = do
  result <- timeout (time * 10 ^ 6) obj
  case result of
    Nothing    -> throwIO $ WSException "connection timed out"
    Just value -> return value

getParameters :: MonadUnliftIO io => WS.Connection -> io LB.ByteString
getParameters conn = do
  dataMsg <- liftIO $ WS.receiveDataMessage conn
  case dataMsg of
    WS.Binary _     -> throwIO $ WSException "invalid protocol"
    WS.Text param _ -> return param

makeConfig :: MonadUnliftIO io => LB.ByteString -> io String
makeConfig params = case RTConfigCreator.jsonToConfig params of
  RTConfigCreator.DecodeFail           -> throwIO $ WSException "bad request"
  RTConfigCreator.IntegrityFail err    -> throwIO $ WSException err
  RTConfigCreator.Config        config -> return config

-- Make new MVar, register it to daemon, takes ID of event, and enqueue with it
registerEvent :: MonadUnliftIO io => WSData -> String -> io (MVar WSMessage, Int)
registerEvent (WSData (dbconn, chan)) config = do
  mvar <- newEmptyMVar
  writeChan chan mvar
  ~(MessageId id) <- takeMVar mvar
  result          <- MessageQueue.enqueue id config dbconn
  case result of
    Nothing -> throwIO $ WSException "DB excption occurred: register"
    Just _  -> putStrLn ("Job #" ++ show id ++ " queued.") >> return (mvar, id)

unregisterEvent :: MonadUnliftIO io => WSData -> Int -> io ()
unregisterEvent (WSData (dbconn, _)) id = do
  result <- MessageQueue.dequeue id dbconn
  case result of
    Nothing    -> throwIO $ WSException "DB excption occurred: unregister"
    Just True  -> putStrLn ("Job #" ++ show id ++ " dequeued.") >> return ()
    Just False -> return () -- TODO terminate running process

-- Receive daemon message, and send it to client
doProxy :: MonadUnliftIO io => WS.Connection -> MVar WSMessage -> io ()
doProxy conn messageMVar = do
  liftIO $ WS.forkPingThread conn 5
  let loop = doProxy conn messageMVar
  message <- takeMVar messageMVar
  case message of
    RemainingQueue n ->
      send conn ("Job queued: " ++ show n ++ " jobs remaining") >> loop
    ProcessStarted     -> send conn "Processing image..." >> loop
    ProcessFinished id -> send conn ("Finished: " ++ show id)
      >> liftIO (WS.sendClose conn (LBC.pack "end"))
    ProcessFailed -> send conn "Finished: -1"
      >> liftIO (WS.sendClose conn (LBC.pack "endfail"))

-- Send close request to client
closeRequest :: MonadUnliftIO io => WS.Connection -> io ()
closeRequest conn = do
  liftIO $ WS.sendClose conn (LBC.pack "end")
  _ <- liftIO $ timeoutWrapper 1 $ WS.receiveDataMessage conn
  return ()

-- Send string to client
send :: MonadUnliftIO io => WS.Connection -> String -> io ()
send conn msg = liftIO $ WS.sendTextData conn $ LBC.pack msg

