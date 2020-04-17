module Sonowz.Raytrace.Daemon.Types where

import Relude
import UnliftIO (MonadUnliftIO(..))
import qualified Database.PostgreSQL.Simple as PGS

import Sonowz.Raytrace.Core.Has (Has(..), MonadHas(..))


newtype DaemonApp a = DaemonApp (ReaderT DaemonAppEnv IO a)
  deriving (Functor, Generic)
  deriving (Applicative, Monad, MonadIO, MonadUnliftIO) via ReaderT DaemonAppEnv IO

instance Has field DaemonAppEnv => MonadHas field DaemonApp where
  grab = DaemonApp $ obtain <$> ask

data DaemonAppEnv = DaemonAppEnv
  { ePgConn :: PGS.Connection
  }
instance Has PGS.Connection DaemonAppEnv where
  obtain = ePgConn
