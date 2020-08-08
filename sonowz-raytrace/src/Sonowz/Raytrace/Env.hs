module Sonowz.Raytrace.Env where

import qualified Network.Wai.Handler.Warp as Warp

import Sonowz.Core.DB.Pool (DBConnPool)

data Env = Env
  { envWarpPort :: Warp.Port
  , envPgConnection :: DBConnPool
  }
