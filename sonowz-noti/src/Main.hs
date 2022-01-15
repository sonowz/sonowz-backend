module Main where

import qualified Database.PostgreSQL.Simple as PGS
import Network.Mail.Mime (Address(..))
import Options.Applicative
import Sonowz.Core.DB.Pool (createConnPool)
import Sonowz.Noti.App (runApp)
import Sonowz.Noti.Env (Env(..))
import Sonowz.Noti.Imports
import Sonowz.Noti.Notification.Handler.Email (EmailConfig(..))
import System.IO (BufferMode(LineBuffering), hSetBuffering)


data Config = Config EmailConfig PGS.ConnectInfo

emailConfigP :: Parser EmailConfig
emailConfigP = do
  emailConfigEmail    <- Address Nothing <$> strOption (long "email")
  emailConfigPassword <- strOption (long "emailpasswd")
  emailConfigHostname <- strOption (long "emailhostname" <> value "sonowz.me")
  return EmailConfig { .. }


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
configP = Config <$> emailConfigP <*> connectInfoP

opts :: ParserInfo Config
opts = info (helper <*> configP) (fullDesc <> progDesc "Notification generator")

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- For debugging
  hSetBuffering stderr LineBuffering

  (Config emailConfig pgConnectInfo) <- execParser opts
  dbPool                             <- createConnPool pgConnectInfo
  let env = Env emailConfig dbPool
  runApp env

