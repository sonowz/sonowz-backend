module Main where

import Control.Concurrent (forkIO)
import Data.Version (makeVersion)
import Database.PostgreSQL.Simple qualified as PGS
import OptEnvConf
import Sonowz.Core.Config.Common (pPGSConnectInfo, pWarpPort)
import Sonowz.Core.DB.Pool (createConnPool)
import Sonowz.Core.Imports
import Sonowz.Core.Web.WebAppEnv (WebAppEnv (..), defaultWebAppEnv)
import Sonowz.NewsCombinator.App.RuleWorker (runRuleWorker)
import Sonowz.NewsCombinator.App.Web (runServer)
import Sonowz.NewsCombinator.Env (Env (..))

data Config = Config WebAppEnv PGS.ConnectInfo Int

pWorkerInterval :: Parser Int
pWorkerInterval =
  setting
    [ help "Time interval between worker runs (in seconds)",
      reader auto,
      long "worker-interval",
      short 't',
      option,
      metavar "SECONDS",
      value 3600
    ]

pWebEnv :: Parser WebAppEnv
pWebEnv = (\port -> defaultWebAppEnv {eWebPort = port}) <$> pWarpPort

pConfig :: Parser Config
pConfig = Config <$> pWebEnv <*> pPGSConnectInfo <*> pWorkerInterval

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- For debugging
  hSetBuffering stderr LineBuffering

  (Config webEnv pgConnectInfo workerInterval) <-
    runParser (makeVersion []) "News notification service" pConfig
  dbPool <- createConnPool pgConnectInfo
  let env = Env dbPool workerInterval

  void $ forkIO $ void $ runRuleWorker env
  runServer webEnv env
