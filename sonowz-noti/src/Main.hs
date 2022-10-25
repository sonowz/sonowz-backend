module Main where

import Database.PostgreSQL.Simple qualified as PGS
import Network.Mail.Mime (Address (..))
import Options.Applicative
import Sonowz.Core.DB.Pool (createConnPool)
import Sonowz.Core.Options.Applicative.Common (pPGSConnectInfo)
import Sonowz.Noti.App (runApp)
import Sonowz.Noti.Env (Env (..))
import Sonowz.Noti.Imports
import Sonowz.Noti.Notification.Handler.Email (EmailConfig (..))

data Config = Config EmailConfig PGS.ConnectInfo

pEmailConfig :: Parser EmailConfig
pEmailConfig = do
  emailConfigEmail <- Address Nothing <$> strOption (long "email")
  emailConfigPassword <- strOption (long "emailpasswd")
  emailConfigHostname <- strOption (long "emailhostname" <> value "smtp.gmail.com")
  emailConfigPort <- option auto (long "emailport" <> value 587)
  return EmailConfig {..}

pConfig :: Parser Config
pConfig = Config <$> pEmailConfig <*> pPGSConnectInfo

opts :: ParserInfo Config
opts = info (helper <*> pConfig) (fullDesc <> progDesc "Notification generator")

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- For debugging
  hSetBuffering stderr LineBuffering

  (Config emailConfig pgConnectInfo) <- execParser opts
  dbPool <- createConnPool pgConnectInfo
  let env = Env emailConfig dbPool
  runApp env
