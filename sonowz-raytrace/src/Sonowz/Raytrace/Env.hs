module Sonowz.Raytrace.Env where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Database.PostgreSQL.Simple as PGS

import Sonowz.Raytrace.Core.Has (Has(..))

data Env = Env
  { envWarpPort :: Warp.Port
  , envPgConnection :: PGS.Connection
  }

instance Has Warp.Port Env where obtain = envWarpPort
instance Has PGS.Connection Env where obtain = envPgConnection

