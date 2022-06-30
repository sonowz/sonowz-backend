module Sonowz.Core.Options.Applicative.Common
  ( pWarpPort,
    pPGSConnectInfo,
  )
where

import Database.PostgreSQL.Simple qualified as PGS
import Network.Wai.Handler.Warp (Port)
import Options.Applicative
import Sonowz.Core.Imports

pWarpPort :: Parser Port
pWarpPort = option (auto >>= checkPort) (long "port" <> short 'p' <> value 80 <> showDefault)
  where
    checkPort port = if 0 < port && port < 90000 then return port else fail "Invalid port number!"

pPGSConnectInfo :: Parser PGS.ConnectInfo
pPGSConnectInfo = do
  let def = PGS.defaultConnectInfo
  connectHost <-
    strOption
      (long "pghost" <> short 'h' <> value (PGS.connectHost def) <> showDefault)
  connectPort <-
    option
      auto
      (long "pgport" <> short 'P' <> value (PGS.connectPort def) <> showDefault)
  connectUser <-
    strOption
      (long "pguser" <> short 'u' <> value (PGS.connectUser def) <> showDefault)
  connectPassword <- strOption (long "pgpasswd" <> short 'w')
  connectDatabase <-
    strOption
      (long "pgdatabase" <> short 'd' <> value (PGS.connectDatabase def) <> showDefault)
  return PGS.ConnectInfo {..}
