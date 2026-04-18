module Main where

import Data.Version (makeVersion)
import Database.PostgreSQL.Simple qualified as PGS
import Network.Mail.Mime (Address (..))
import OptEnvConf
import Sonowz.Core.Config.Common (pPGSConnectInfo)
import Sonowz.Core.DB.Pool (createConnPool)
import Sonowz.Noti.App (runApp)
import Sonowz.Noti.Env (Env (..))
import Sonowz.Noti.Imports
import Sonowz.Noti.Notification.Handler.Email (EmailConfig (..))

data Config = Config EmailConfig PGS.ConnectInfo

pEmailConfig :: Parser EmailConfig
pEmailConfig = do
  emailConfigEmail <-
    Address Nothing
      <$> setting
        [ help "Email address for notifications",
          reader str,
          long "email",
          env "NOTI_EMAIL_ADDRESS"
        ]
  emailConfigPassword <-
    setting
      [ help "Email password",
        reader str,
        long "emailpasswd",
        env "NOTI_EMAIL_PASSWORD"
      ]
  emailConfigHostname <-
    setting
      [ help "Email SMTP hostname",
        reader str,
        long "emailhostname",
        value "smtp.gmail.com"
      ]
  emailConfigPort <-
    setting
      [ help "Email SMTP port",
        reader auto,
        long "emailport",
        value 587
      ]
  return EmailConfig {..}

pConfig :: Parser Config
pConfig = Config <$> pEmailConfig <*> pPGSConnectInfo

main :: IO Void
main = do
  hSetBuffering stdout LineBuffering -- For debugging
  hSetBuffering stderr LineBuffering

  (Config emailConfig pgConnectInfo) <-
    runParser (makeVersion []) "Notification generator" pConfig
  dbPool <- createConnPool pgConnectInfo
  let env = Env emailConfig dbPool
  runApp env
