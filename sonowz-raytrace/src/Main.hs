module Main where

import Network.Wai.Handler.Warp (Port)
import Options.Applicative
import Servant.Server (serve)
import System.IO (hSetBuffering, BufferMode(LineBuffering))
import qualified Database.PostgreSQL.Simple as PGS
import qualified Network.Wai.Handler.Warp as Warp

import Sonowz.Raytrace.Imports

import Sonowz.Raytrace.Env (Env(..))
import Sonowz.Core.DB.Pool (createConnPool)
import Sonowz.Raytrace.App.Daemon (forkDaemon)
import Sonowz.Raytrace.App.Web (api, server)


data Config = Config Port PGS.ConnectInfo

warpPortP :: Parser Port
warpPortP = option (auto >>= checkPort) (long "port" <> short 'p' <> value 80)
  where checkPort port = if 0 < port && port < 90000 then return port else empty

connectInfoP :: Parser PGS.ConnectInfo
connectInfoP = do
  let def = PGS.defaultConnectInfo
  connectHost     <- strOption (long "pghost" <> short 'h' <> value (PGS.connectHost def))
  connectPort     <- option auto (long "pgport" <> short 'P' <> value (PGS.connectPort def))
  connectUser     <- strOption (long "pguser" <> short 'u' <> value (PGS.connectUser def))
  connectPassword <- strOption (long "pgpasswd" <> short 'w')
  connectDatabase <- strOption (long "pgdatabase" <> short 'd' <> value (PGS.connectDatabase def))
  return PGS.ConnectInfo { .. }

configP :: Parser Config
configP = Config <$> warpPortP <*> connectInfoP

opts :: ParserInfo Config
opts = info (helper <*> configP) (fullDesc <> progDesc "Raytrace backend server")


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- For debugging
  hSetBuffering stderr LineBuffering

  (Config warpPort pgConnectInfo) <- execParser opts

  pgConnection                    <- createConnPool pgConnectInfo
  let env = Env warpPort pgConnection

  logInfoIO "Forking daemon thread..."
  forkDaemon env

  logInfoIO "Starting servant server..."
  let waiApp = serve api (server env)
  Warp.run warpPort waiApp
