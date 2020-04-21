module Sonowz.Raytrace.Daemon.Types where

import Relude
import UnliftIO (MonadUnliftIO(..))
import UnliftIO.Async (Async)
import qualified Database.PostgreSQL.Simple as PGS

import Sonowz.Raytrace.Core.Has (Has(..), MonadHas(..))
import Sonowz.Raytrace.Monad.MQueue (MonadMQueue(..))
import Sonowz.Raytrace.Monad.MQueue.DaemonDB (enqueueDaemonDB, dequeueDaemonDB)
import Sonowz.Raytrace.Monad.MQueue.Db.Types (ServantId, DaemonMessage)
import Sonowz.Raytrace.Monad.MQueue.IORef (IORefQueue, enqueueIORef, dequeueIORef)
import Sonowz.Raytrace.RaytraceConfig (Config)
import qualified Sonowz.Raytrace.Daemon.RunnerScript as Script


data RunInfo = RunInfo ServantId Config deriving (Show, Read)
type RunnerProcess = Async Script.ShellResult
newtype CurrentRunInfo = CurrentRunInfo (IORef (RunInfo, RunnerProcess))

newtype DaemonApp a = DaemonApp (ReaderT DaemonAppEnv IO a)
  deriving (Functor, Generic)
  deriving (Applicative, Monad, MonadIO, MonadUnliftIO) via ReaderT DaemonAppEnv IO

instance MonadMQueue DaemonMessage DaemonApp where
  enqueue = enqueueDaemonDB
  dequeue = dequeueDaemonDB

instance MonadMQueue RunInfo DaemonApp where
  enqueue = enqueueIORef
  dequeue = dequeueIORef

instance Has field DaemonAppEnv => MonadHas field DaemonApp where
  grab = DaemonApp $ obtain <$> ask

data DaemonAppEnv = DaemonAppEnv
  { ePgConn :: PGS.Connection
  , eRunInfoQueue :: IORefQueue RunInfo
  , eCurrentRunInfo :: CurrentRunInfo
  }
instance Has PGS.Connection DaemonAppEnv where
  obtain = ePgConn
instance Has (IORefQueue RunInfo) DaemonAppEnv where
  obtain = eRunInfoQueue
instance Has CurrentRunInfo DaemonAppEnv where
  obtain = eCurrentRunInfo
initRunInfoQueue :: MonadIO m => m (IORefQueue RunInfo)
initRunInfoQueue = newIORef ([] :: [RunInfo])
emptyCurrentRunInfo :: MonadIO m => m CurrentRunInfo
emptyCurrentRunInfo =
  CurrentRunInfo <$> newIORef (error "Attempt to access uninitialized CurrentRunInfo")
