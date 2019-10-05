{-# LANGUAGE OverloadedStrings #-}

module Raytrace.Websocket (websocketApp, initWSData, WSData(WSData), WSMessage(..)) where

import           System.Timeout
import           Control.Monad
import           Control.Exception
import           Control.Concurrent
import           Control.Concurrent.MVar
import qualified Network.WebSockets      as WS
import qualified Network.WebSockets.Snap as WS
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LBC

import qualified Raytrace.MessageQueue as MessageQueue
import qualified Raytrace.RTConfigCreator as RTConfigCreator

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


initWSData :: IO WSData
initWSData = do
  conn <- MessageQueue._init
  chan <- newChan
  return $ WSData (conn, chan)

-- Websocket entry point
websocketApp :: WSData -> WS.ServerApp
websocketApp wsdata pending = do
  conn <- WS.acceptRequest pending
  catch (afterConnection conn) (\(WSException err) -> send conn (show err))
  threadDelay (10^6)
  closeRequest conn where
    afterConnection :: WS.Connection -> IO ()
    afterConnection conn = do
      params <- timeoutWrapper 3 (getParameters conn)
      config <- makeConfig params
      (messageMVar, id) <- registerEvent wsdata config
      _ <- forkIO (catchConnError wsdata id $ doProxy conn messageMVar)
      catchConnError wsdata id $ ping conn

-- Receive ping from client
ping :: WS.Connection -> IO ()
ping conn = do
  forever $ do
    WS.receive conn
    threadDelay (10 * 10^6)

---- Minor functions ----

-- Connection error wrapper & handler
catchConnError :: WSData -> Int -> IO a -> IO a
catchConnError wsdata id work = catch work (handler wsdata id) where
  handler :: WSData -> Int -> WS.ConnectionException -> IO a
  handler wsdata id e = unregisterEvent wsdata id >> throw (WSException "client closed")

-- Timeout setter wrapper
timeoutWrapper :: Int -> IO a -> IO a
timeoutWrapper time obj = do
  result <- timeout (time * 10^6) obj
  case result of
    Nothing -> throw $ WSException "connection timed out"
    Just value -> return value

getParameters :: WS.Connection -> IO LB.ByteString
getParameters conn = do
  dataMsg <- WS.receiveDataMessage conn
  case dataMsg of
    WS.Binary _ -> throw $ WSException "invalid protocol"
    WS.Text param _ -> return param

makeConfig :: LB.ByteString -> IO String
makeConfig params =
  case RTConfigCreator.jsonToConfig params of
    RTConfigCreator.DecodeFail -> throw $ WSException "bad request"
    RTConfigCreator.IntegrityFail err -> throw $ WSException err
    RTConfigCreator.Config config -> return config

-- Make new MVar, register it to daemon, takes ID of event, and enqueue with it
registerEvent :: WSData -> String -> IO (MVar WSMessage, Int)
registerEvent (WSData (dbconn, chan)) config = do
  mvar <- newEmptyMVar
  writeChan chan mvar
  (MessageId id) <- takeMVar mvar
  result <- MessageQueue.enqueue dbconn id config
  case result of
    Nothing -> throw $ WSException "DB excption occurred: register"
    Just _ -> putStrLn ("Job #" ++ show id ++ " queued.") >> return (mvar, id)

unregisterEvent :: WSData -> Int -> IO ()
unregisterEvent (WSData (dbconn, _)) id = do
  result <- MessageQueue.dequeue dbconn id
  case result of
    Nothing -> throw $ WSException "DB excption occurred: unregister"
    Just True -> putStrLn ("Job #" ++ show id ++ " dequeued.") >> return ()
    Just False -> return () -- TODO terminate running process

-- Receive daemon message, and send it to client
doProxy :: WS.Connection -> MVar WSMessage -> IO ()
doProxy conn messageMVar = do
  WS.forkPingThread conn 5
  let loop = doProxy conn messageMVar
  message <- takeMVar messageMVar
  case message of
    RemainingQueue n -> send conn ("Job queued: " ++ show n ++ " jobs remaining") >> loop
    ProcessStarted -> send conn ("Processing image...") >> loop
    ProcessFinished id -> send conn ("Finished: " ++ show id) >> WS.sendClose conn (LBC.pack "end")
    ProcessFailed -> send conn ("Finished: -1") >> WS.sendClose conn (LBC.pack "endfail")

-- Send close request to client
closeRequest :: WS.Connection -> IO ()
closeRequest conn = do
  WS.sendClose conn (LBC.pack "end") 
  _ <- timeoutWrapper 1 $ WS.receiveDataMessage conn
  return ()

-- Send string to client
send :: WS.Connection -> String -> IO ()
send conn msg = WS.sendTextData conn $ LBC.pack msg

