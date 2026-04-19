module Main where

import Control.Concurrent (forkIO)
import Data.Version (makeVersion)
import Database.PostgreSQL.Simple qualified as PGS
import Network.Mail.Mime (Address (..))
import OptEnvConf
import Sonowz.Core.Config.Common (pPGSConnectInfo, pWarpPort)
import Sonowz.Core.DB.Pool (createConnPool)
import Sonowz.Core.Web.WebAppEnv (WebAppEnv (..), defaultWebAppEnv)
import Sonowz.Noti.App.NotiWorker (runNotiWorker)
import Sonowz.Noti.App.Web (runServer)
import Sonowz.Noti.Env (Env (..))
import Sonowz.Noti.Imports
import Sonowz.Noti.Notification.Handler.Email (EmailConfig (..))

data Config = Config EmailConfig PGS.ConnectInfo WebAppEnv

pEmailConfig :: Parser EmailConfig
pEmailConfig = do
  emailConfigEmail <-
    Address Nothing
      <$> setting
        [ help "Email address for notifications",
          reader str,
          long "email",
          option,
          env "NOTI_EMAIL_ADDRESS",
          metavar "NOTI_EMAIL_ADDRESS"
        ]
  emailConfigPassword <-
    setting
      [ help "Email password",
        reader str,
        long "emailpasswd",
        option,
        env "NOTI_EMAIL_PASSWORD",
        metavar "NOTI_EMAIL_PASSWORD"
      ]
  emailConfigHostname <-
    setting
      [ help "Email SMTP hostname",
        reader str,
        long "emailhostname",
        option,
        metavar "EMAIL_HOSTNAME",
        value "smtp.gmail.com"
      ]
  emailConfigPort <-
    setting
      [ help "Email SMTP port",
        reader auto,
        long "emailport",
        option,
        metavar "EMAIL_PORT",
        value 587
      ]
  return EmailConfig {..}

pWebEnv :: Parser WebAppEnv
pWebEnv = (\port -> defaultWebAppEnv {eWebPort = port}) <$> pWarpPort

pConfig :: Parser Config
pConfig = Config <$> pEmailConfig <*> pPGSConnectInfo <*> pWebEnv

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- For debugging
  hSetBuffering stderr LineBuffering

  (Config emailConfig pgConnectInfo webEnv) <-
    runParser (makeVersion []) "Notification generator" pConfig
  dbPool <- createConnPool pgConnectInfo
  let env = Env emailConfig dbPool
  void $ forkIO $ void $ runNotiWorker env
  runServer webEnv env
