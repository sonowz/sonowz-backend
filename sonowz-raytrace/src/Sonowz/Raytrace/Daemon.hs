{-# LANGUAGE ScopedTypeVariables #-}

module Sonowz.Raytrace.Daemon (main, onQuit) where

import Relude hiding (newEmptyMVar, takeMVar, putMVar)
import System.Exit
import Data.Time
import Data.Map.Strict ((!))
import UnliftIO.Chan
import UnliftIO.Concurrent
import qualified Data.Map.Strict as Map

import qualified Sonowz.Raytrace.MessageQueue as MessageQueue
import qualified Sonowz.Raytrace.DaemonScript as Script
import qualified Sonowz.Raytrace.Web.Websocket as RTWebsocket


type WSMVar = MVar RTWebsocket.WSMessage
type WSDict = Map.Map Int WSMVar

-- Daemon terminate handler
onQuit :: Either SomeException () -> IO ()
onQuit (Left err) = do
  putStrLn "Daemon terminated with error."
  print err
onQuit (Right _) = putStrLn "Daemon terminated successfully."

-- Daemon entry point
main :: RTWebsocket.WSData -> IO ()
main (RTWebsocket.WSData (conn, registerChan)) = do
  -- Make mutable instances
  dictRef <- (newIORef Map.empty :: IO (IORef WSDict))
  idRef <- newIORef 1
  _ <- forkIO $ dictSetter dictRef idRef registerChan

  -- Get job from MessageQueue and execute
  forever $ do
    message <- MessageQueue.popFrontMessage conn
    case message of
      Nothing -> threadDelay 1000000
      Just (pid, config) -> do
        status <- MessageQueue.getStatus conn
        sendStatus dictRef `mapM` status
        sendProcessStart dictRef pid
        success <- doRaytraceProcess pid config
        if success
          then sendProcessFinish dictRef pid
          else sendProcessFail dictRef pid


-- Subprocess which handles new request
dictSetter :: IORef WSDict -> IORef Int -> Chan WSMVar -> IO ()
dictSetter dictRef idRef chan = forever $ do
  wsMVar <- readChan chan
  setDict dictRef idRef wsMVar >>= sendID wsMVar

-- Returns new ID
setDict :: IORef WSDict -> IORef Int -> WSMVar -> IO Int
setDict dictRef idRef wsMVar = do
  pid <- readIORef idRef
  pid' <- return (pid + 1)
  writeIORef idRef pid'
  modifyIORef' dictRef (\dict -> Map.insert pid' wsMVar dict)
  return pid'

getMVarFromDict :: IORef WSDict -> Int -> IO WSMVar
getMVarFromDict dictRef pid = do
  dict <- readIORef dictRef
  return $ dict ! pid


-- Send functions which communicate with RTWebsocket thread

sendID :: WSMVar -> Int -> IO ()
sendID wsMVar pid = putMVar wsMVar (RTWebsocket.MessageId pid)

sendSignal :: RTWebsocket.WSMessage -> IORef WSDict -> Int -> IO ()
sendSignal signal dictRef pid =
  getMVarFromDict dictRef pid >>= (flip putMVar) signal

sendStatus :: IORef WSDict -> (Int, Int) -> IO ()
sendStatus dictRef (pid, n) = sendSignal (RTWebsocket.RemainingQueue n) dictRef pid 

sendProcessFinish :: IORef WSDict -> Int -> IO ()
sendProcessFinish dictRef pid = sendSignal (RTWebsocket.ProcessFinished pid) dictRef pid

sendProcessStart :: IORef WSDict -> Int -> IO ()
sendProcessStart = sendSignal RTWebsocket.ProcessStarted

sendProcessFail :: IORef WSDict -> Int -> IO ()
sendProcessFail = sendSignal RTWebsocket.ProcessFailed

-- Execute Turtle(shellscript) to make raytrace image
doRaytraceProcess :: Int -> String -> IO Bool
doRaytraceProcess pid config = do
  -- TODO make config file
  let raytracePath = "/home/sonowz/packages/raytrace"
  let outputPath = "/home/sonowz/data/www/graphics-demo/image/raytrace"
  shellResult <- Script.raytraceScript pid (toText config) raytracePath outputPath
  time <- fmap show getZonedTime
  case shellResult of
    Script.ShellResult ExitSuccess _ _ -> putTextLn (time <> ": Job #" <> show pid <> " finished.") >> return True
    Script.ShellResult (ExitFailure _) out err -> do
      putTextLn (time <> ": Job #" <> show pid <> " failed.")
      putTextLn out
      putTextLn err
      return False
