module Sonowz.Raytrace.Web.Types where

import Relude
import Servant.API
import Servant.API.WebSocket
import UnliftIO (MonadUnliftIO(..))
import qualified Network.WebSockets as WS

import Sonowz.Raytrace.Core.Has (Has(..), MonadHas(..))
import Sonowz.Raytrace.Core.DB (DBConnPool)
import Sonowz.Raytrace.Monad.MQueue (MonadMQueue(..))
import Sonowz.Raytrace.Monad.MQueue.Db.Types (DaemonMessage)
import Sonowz.Raytrace.Monad.MQueue.DaemonDB (enqueueDaemonDB, dequeueDaemonDB)

type API = RaytraceWsAPI

type RaytraceWsAPI = "wait" :> WebSocket


newtype ServantApp a = ServantApp (ReaderT ServantAppEnv IO a)
  deriving (Functor, Generic)
  deriving (Applicative, Monad, MonadIO, MonadUnliftIO) via ReaderT ServantAppEnv IO

instance MonadMQueue DaemonMessage ServantApp where
  enqueue = enqueueDaemonDB
  dequeue = dequeueDaemonDB

instance Has field ServantAppEnv => MonadHas field ServantApp where
  grab = ServantApp $ obtain <$> ask

data ServantAppEnv = ServantAppEnv
  { eWsConn :: ~WS.Connection -- This may contain undefined connection, so make it lazy
  , ePgConn :: DBConnPool
  }
instance Has WS.Connection ServantAppEnv where
  obtain = eWsConn
instance Has DBConnPool ServantAppEnv where
  obtain = ePgConn
