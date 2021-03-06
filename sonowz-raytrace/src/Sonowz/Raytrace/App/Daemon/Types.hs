module Sonowz.Raytrace.App.Daemon.Types where

import Control.Concurrent.Async (Async)

import Sonowz.Raytrace.Imports
import Sonowz.Raytrace.DB.Types (ServantId)
import Sonowz.Raytrace.RaytraceConfig (Config)
import qualified Sonowz.Raytrace.App.Daemon.RunnerScript as Script


data RunInfo = RunInfo ServantId Config deriving (Show, Read)
data RunnerProcess = RunnerProcess (Async Script.ShellResult)
newtype CurrentRunInfo = CurrentRunInfo (RunInfo, RunnerProcess)
