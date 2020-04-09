module Sonowz.Raytrace.Web.Types where

import Relude
import Servant.API
import Servant.API.WebSocket
import UnliftIO (MonadUnliftIO(..))
import qualified Network.WebSockets as WS
import qualified Database.PostgreSQL.Simple as PGS

import Sonowz.Raytrace.Core.Has (Has(..), MonadHas(..))

type API = RaytraceWsAPI

type RaytraceWsAPI = "wait" :> WebSocket


newtype ServantApp a = ServantApp (ReaderT ServantAppEnv IO a)
  deriving (Functor, Generic)
  deriving (Applicative, Monad, MonadIO, MonadUnliftIO) via ReaderT ServantAppEnv IO

instance Has field ServantAppEnv => MonadHas field ServantApp where
  grab = ServantApp $ obtain <$> ask

data ServantAppEnv = ServantAppEnv
  { eWsConn :: WS.Connection
  , ePgConn :: PGS.Connection
  }
instance Has WS.Connection ServantAppEnv where
  obtain = eWsConn
instance Has PGS.Connection ServantAppEnv where
  obtain = ePgConn
