module Main where

import Control.Concurrent (forkIO)
import Database.PostgreSQL.Simple qualified as PGS
import Options.Applicative
import Sonowz.Core.DB.Pool (createConnPool)
import Sonowz.Core.Imports
import Sonowz.Core.Options.Applicative.Common (pPGSConnectInfo)
import Sonowz.Core.Web.WebAppEnv (WebAppEnv (..), defaultWebAppEnv)
import Sonowz.NewsCombinator.App.RuleWorker (runRuleWorker)
import Sonowz.NewsCombinator.App.Web
import Sonowz.NewsCombinator.Env (Env (..))
import System.IO (BufferMode (LineBuffering), hSetBuffering)

data Config = Config WebAppEnv PGS.ConnectInfo Int

pWorkerInterval :: Parser Int
pWorkerInterval = option auto (long "worker-interval" <> short 't' <> metavar "SECONDS" <> value 3600 <> help helpMsg)
  where
    helpMsg = "Time interval between worker runs (in seconds)"

pWebEnv :: Parser WebAppEnv
pWebEnv = do
  port <- option (auto >>= checkPort) (long "port" <> short 'p' <> value 80)
  return $ defaultWebAppEnv {eWebPort = port}
  where
    checkPort port = if 0 < port && port < 90000 then return port else empty

pConfig :: Parser Config
pConfig = Config <$> pWebEnv <*> pPGSConnectInfo <*> pWorkerInterval

opts :: ParserInfo Config
opts = info (helper <*> pConfig) (fullDesc <> progDesc "News notification service")

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- For debugging
  hSetBuffering stderr LineBuffering

  (Config webEnv pgConnectInfo workerInterval) <- execParser opts
  dbPool <- createConnPool pgConnectInfo
  let env = Env dbPool workerInterval

  void $ forkIO $ runRuleWorker env
  runServer webEnv env
