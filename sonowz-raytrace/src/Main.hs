module Main where

import Database.PostgreSQL.Simple qualified as PGS
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Handler.Warp qualified as Warp
import Options.Applicative
import Servant.Server (serve)
import Sonowz.Core.DB.Pool (createConnPool)
import Sonowz.Core.Options.Applicative.Common (pPGSConnectInfo, pWarpPort)
import Sonowz.Raytrace.App.Daemon (forkDaemon)
import Sonowz.Raytrace.App.Web (api, server)
import Sonowz.Raytrace.Env (Env (..))
import Sonowz.Raytrace.Imports

data Config = Config Port PGS.ConnectInfo

pConfig :: Parser Config
pConfig = Config <$> pWarpPort <*> pPGSConnectInfo

opts :: ParserInfo Config
opts = info (helper <*> pConfig) (fullDesc <> progDesc "Raytrace backend server")

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- For debugging
  hSetBuffering stderr LineBuffering

  (Config warpPort pgConnectInfo) <- execParser opts

  pgConnection <- createConnPool pgConnectInfo
  let env = Env warpPort pgConnection

  logInfoIO "Forking daemon thread..."
  forkDaemon env

  logInfoIO "Starting servant server..."
  let waiApp = serve api (server env)
  Warp.run warpPort waiApp
