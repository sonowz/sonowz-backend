module Sonowz.Raytrace.App.Daemon
  ( forkDaemon,
  )
where

import Sonowz.Raytrace.App.Daemon.Process (forkRaytraceDaemon)
import Sonowz.Raytrace.Env (Env (..))
import Sonowz.Raytrace.Imports

forkDaemon :: Env -> IO ()
forkDaemon Env {..} = forkRaytraceDaemon envPgConnection
