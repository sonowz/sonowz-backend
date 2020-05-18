module Sonowz.Raytrace.Env where

import qualified Network.Wai.Handler.Warp as Warp

import Sonowz.Raytrace.DB.Pool (DBConnPool)

data Env = Env
  { envWarpPort :: Warp.Port
  , envPgConnection :: DBConnPool
  }
