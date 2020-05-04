module Sonowz.Raytrace.Env where

import qualified Network.Wai.Handler.Warp as Warp

import Sonowz.Raytrace.Core.Has (Has(..))
import Sonowz.Raytrace.Core.DB (DBConnPool)

data Env = Env
  { envWarpPort :: Warp.Port
  , envPgConnection :: DBConnPool
  }

instance Has Warp.Port Env where
  obtain = envWarpPort
instance Has DBConnPool Env where
  obtain = envPgConnection

