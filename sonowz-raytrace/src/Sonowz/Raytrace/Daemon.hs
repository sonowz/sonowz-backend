{-# LANGUAGE ScopedTypeVariables #-}

module Raytrace.Daemon (main, onQuit) where

import System.Process
import System.Exit
import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.Time
import Data.IORef
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map

import qualified Raytrace.MessageQueue as MessageQueue
import qualified Raytrace.Websocket as RTWebsocket


type WSMVar = MVar RTWebsocket.WSMessage
type WSDict = Map.Map Int WSMVar

-- Daemon terminate handler
onQuit :: Either SomeException () -> IO ()
onQuit (Left err) = do
  putStrLn "Daemon terminated with error."
  putStrLn $ show err
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
      Nothing -> threadDelay (10^6)
      Just (id, config) -> do
        status <- MessageQueue.getStatus conn
        sendStatus dictRef `mapM` status
        sendProcessStart dictRef id
        success <- doRaytraceProcess id config
        if success
          then sendProcessFinish dictRef id
          else sendProcessFail dictRef id


-- Subprocess which handles new request
dictSetter :: IORef WSDict -> IORef Int -> Chan WSMVar -> IO ()
dictSetter dictRef idRef chan = do
  forever $ do
    wsMVar <- readChan chan
    setDict dictRef idRef wsMVar >>= sendID wsMVar

-- Returns new ID
setDict :: IORef WSDict -> IORef Int -> WSMVar -> IO Int
setDict dictRef idRef wsMVar = do
  id <- readIORef idRef
  id <- return (id + 1)
  writeIORef idRef id
  modifyIORef' dictRef (\dict -> Map.insert id wsMVar dict)
  return id

getMVarFromDict :: IORef WSDict -> Int -> IO WSMVar
getMVarFromDict dictRef id = do
  dict <- readIORef dictRef
  return $ dict ! id


-- Send functions which communicate with RTWebsocket thread

sendID :: WSMVar -> Int -> IO ()
sendID wsMVar id = putMVar wsMVar (RTWebsocket.MessageId id)

sendSignal :: RTWebsocket.WSMessage -> IORef WSDict -> Int -> IO ()
sendSignal signal dictRef id =
  getMVarFromDict dictRef id >>= (flip putMVar) signal

sendStatus :: IORef WSDict -> (Int, Int) -> IO ()
sendStatus dictRef (id, n) = sendSignal (RTWebsocket.RemainingQueue n) dictRef id 

sendProcessFinish :: IORef WSDict -> Int -> IO ()
sendProcessFinish dictRef id = sendSignal (RTWebsocket.ProcessFinished id) dictRef id

sendProcessStart :: IORef WSDict -> Int -> IO ()
sendProcessStart = sendSignal RTWebsocket.ProcessStarted

sendProcessFail :: IORef WSDict -> Int -> IO ()
sendProcessFail = sendSignal RTWebsocket.ProcessFailed

-- Execute shell script to make raytrace image
doRaytraceProcess :: Int -> String -> IO Bool
doRaytraceProcess id config = do
  -- TODO make config file
  let raytracePath = "../../raytrace/"
  let outputPath = "~/www/graphics-demo/image/raytrace/"
  let arguments = [show id, config, raytracePath, outputPath]
  (exitCode, out, err) <- readProcessWithExitCode "./sh/RTExecuter.sh" arguments ""
  time <- fmap show getZonedTime
  case exitCode of
    ExitSuccess -> putStrLn (time ++ ": Job #" ++ show id ++ " finished.") >> return True
    ExitFailure _ -> do
      putStrLn (time ++ ": Job #" ++ show id ++ " failed.")
      putStrLn out
      putStrLn err
      return False
