module Main where

import Data.Version (makeVersion)
import Database.PostgreSQL.Simple qualified as PGS
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Handler.Warp qualified as Warp
import OptEnvConf
import Servant.Server (serve)
import Sonowz.Core.Config.Common (pPGSConnectInfo, pWarpPort)
import Sonowz.Core.DB.Pool (createConnPool)
import Sonowz.Raytrace.App.Daemon (forkDaemon)
import Sonowz.Raytrace.App.Web (api, server)
import Sonowz.Raytrace.Env (Env (..))
import Sonowz.Raytrace.Imports

data Config = Config Port PGS.ConnectInfo

pConfig :: Parser Config
pConfig = Config <$> pWarpPort <*> pPGSConnectInfo

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- For debugging
  hSetBuffering stderr LineBuffering

  (Config warpPort pgConnectInfo) <-
    runParser (makeVersion []) "Raytrace backend server" pConfig

  pgConnection <- createConnPool pgConnectInfo
  let env = Env warpPort pgConnection

  logInfoIO "Forking daemon thread..."
  forkDaemon env

  logInfoIO "Starting servant server..."
  let waiApp = serve api (server env)
  Warp.run warpPort waiApp
