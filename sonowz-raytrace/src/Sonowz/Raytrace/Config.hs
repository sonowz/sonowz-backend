module Sonowz.Raytrace.Config where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Database.PostgreSQL.Simple as PGS

data ProgramConfig = ProgramConfig
  { warpPort :: Warp.Port
  , pgConnectInfo :: PGS.ConnectInfo
  }
