module Sonowz.Core.Config.Common
  ( pWarpPort,
    pPGSConnectInfo,
  )
where

import Database.PostgreSQL.Simple qualified as PGS
import Network.Wai.Handler.Warp (Port)
import OptEnvConf
import Sonowz.Core.Imports

pWarpPort :: Parser Port
pWarpPort =
  setting
    [ help "Port number for the web server",
      reader auto,
      long "port",
      short 'p',
      option,
      metavar "PORT",
      value 80
    ]
    & checkEither (\port -> if 0 < port && port < 90000 then Right port else Left "Invalid port number!")

pPGSConnectInfo :: Parser PGS.ConnectInfo
pPGSConnectInfo = do
  let def = PGS.defaultConnectInfo
  connectHost <-
    setting
      [ help "PostgreSQL host",
        reader str,
        long "pghost",
        short 'h',
        option,
        env "PGHOST",
        metavar "PGHOST",
        value (PGS.connectHost def)
      ]
  connectPort <-
    setting
      [ help "PostgreSQL port",
        reader auto,
        long "pgport",
        short 'P',
        option,
        env "PGPORT",
        metavar "PGPORT",
        value (PGS.connectPort def)
      ]
  connectUser <-
    setting
      [ help "PostgreSQL user",
        reader str,
        long "pguser",
        short 'u',
        option,
        env "PGUSER",
        metavar "PGUSER",
        value (PGS.connectUser def)
      ]
  connectPassword <-
    setting
      [ help "PostgreSQL password",
        reader str,
        long "pgpasswd",
        short 'w',
        option,
        env "PGPASSWD",
        metavar "PGPASSWD"
      ]
  connectDatabase <-
    setting
      [ help "PostgreSQL database name",
        reader str,
        long "pgdatabase",
        short 'd',
        option,
        env "PGDATABASE",
        metavar "PGDATABASE",
        value (PGS.connectDatabase def)
      ]
  return PGS.ConnectInfo {..}
