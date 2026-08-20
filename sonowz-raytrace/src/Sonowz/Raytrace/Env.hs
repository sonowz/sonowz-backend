module Sonowz.Raytrace.Env where

import Network.Wai.Handler.Warp qualified as Warp
import Sonowz.Core.DB.Pool (DBConnPool)

data Env = Env
  { warpPort :: Warp.Port,
    pgConnection :: DBConnPool
  }
