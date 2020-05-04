module Main where

import Relude
import Network.Wai.Handler.Warp (Port, run)
import Options.Applicative
import Servant.Server (serve)
import UnliftIO.IO (hSetBuffering, BufferMode(LineBuffering))
import qualified Database.PostgreSQL.Simple as PGS

import Sonowz.Raytrace.Env (Env(..))
import qualified Sonowz.Raytrace.Daemon as RTDaemon
import qualified Sonowz.Raytrace.Web as RTServant


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

  pgConnection                    <- PGS.connect pgConnectInfo
  let env = Env warpPort pgConnection

  putTextLn "Forking daemon thread..."
  RTDaemon.forkDaemon env

  putTextLn "Starting servant server..."
  let waiApp = serve RTServant.api (RTServant.server env)
  run warpPort waiApp
