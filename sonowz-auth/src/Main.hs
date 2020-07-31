{-# LANGUAGE QuasiQuotes #-}
module Main where

import Network.HTTP.Client.TLS (newTlsManager)
import Network.Wai.Handler.Warp (Port)
import Options.Applicative
import Servant.Server (serve, hoistServer)
import System.IO (hSetBuffering, BufferMode(LineBuffering))
import URI.ByteString.QQ (uri)
import qualified Database.PostgreSQL.Simple as PGS
import qualified Network.Wai.Handler.Warp as Warp

import Sonowz.Auth.Imports
import Sonowz.Auth.App.Web (api, server, runWithEffects)
import Sonowz.Auth.OAuth (GoogleAppInfo(..))
import Sonowz.Core.DB.Pool (createConnPool)
import Sonowz.Core.Web.WebAppEnv (WebAppEnv(..))

-- TODO: parse Google app info & file config option

data Config = Config Port PGS.ConnectInfo GoogleAppInfo

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

googleAppInfoP :: Parser GoogleAppInfo
googleAppInfoP = do
  appId     <- strOption (long "gappid")
  appSecret <- strOption (long "gappsecret")
  return GoogleAppInfo { .. }

configP :: Parser Config
configP = Config <$> warpPortP <*> connectInfoP <*> googleAppInfoP

opts :: ParserInfo Config
opts = info (helper <*> configP) (fullDesc <> progDesc "Raytrace backend server")

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- For debugging
  hSetBuffering stderr LineBuffering

  (Config warpPort pgConnectInfo gAppInfo) <- execParser opts
  dbPool                                   <- createConnPool pgConnectInfo
  tlsManager                               <- newTlsManager
  let webappEnv = WebAppEnv [uri|https://sonowz.me|]

  let
    waiApp = serve api
      $ hoistServer api (runWithEffects webappEnv tlsManager dbPool) (server webappEnv gAppInfo)
  Warp.run warpPort waiApp
