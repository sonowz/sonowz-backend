module Sonowz.Raytrace.Daemon
  ( forkDaemon
  )
where

import Relude
import UnliftIO.Async (async)

import Sonowz.Raytrace.Env (Env(..))
import Sonowz.Raytrace.Monad.MQueue.DaemonDB ()
import Sonowz.Raytrace.Daemon.Types
import Sonowz.Raytrace.Daemon.Raytrace (forkRaytraceDaemon)


-- This will fork threads and return immediately
forkDaemon :: Env -> IO ()
forkDaemon env = void $ async $ runDaemonAppAsIO env forkRaytraceDaemon

runDaemonAppAsIO :: Env -> DaemonApp a -> IO a
runDaemonAppAsIO Env {..} (DaemonApp app) = do
  let ePgConn = envPgConnection
  eRunInfoQueue   <- initRunInfoQueue
  eCurrentRunInfo <- emptyCurrentRunInfo
  runReaderT app DaemonAppEnv { .. }
