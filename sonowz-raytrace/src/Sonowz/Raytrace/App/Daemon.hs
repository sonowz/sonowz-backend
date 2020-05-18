module Sonowz.Raytrace.App.Daemon
  ( forkDaemon
  )
where

import Sonowz.Raytrace.Imports
import Sonowz.Raytrace.App.Daemon.Process (forkRaytraceDaemon)
import Sonowz.Raytrace.Env (Env(..))

forkDaemon :: Env -> IO ()
forkDaemon Env {..} = forkRaytraceDaemon envPgConnection
