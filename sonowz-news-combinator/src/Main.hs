module Main where

import Control.Concurrent (forkIO)
import Data.Version (makeVersion)
import Database.PostgreSQL.Simple qualified as PGS
import OptEnvConf
import Sonowz.Core.Config.Common (pPGSConnectInfo, pWarpPort)
import Sonowz.Core.DB.Pool (createConnPool)
import Sonowz.Core.Imports
import Sonowz.Core.Llm.Effect (LlmEnv (..))
import Sonowz.Core.Web.WebAppEnv (WebAppEnv (..), defaultWebAppEnv)
import Sonowz.NewsCombinator.App.RuleWorker (runRuleWorker)
import Sonowz.NewsCombinator.App.Web (runServer)
import Sonowz.NewsCombinator.Env (Env (..))

data Config = Config WebAppEnv PGS.ConnectInfo Int LlmEnv

pWorkerInterval :: Parser Int
pWorkerInterval =
  setting
    [ help "Time interval between worker runs (in seconds)",
      reader auto,
      long "worker-interval",
      short 't',
      option,
      metavar "SECONDS",
      value 2592000 -- 30 days
    ]

pWebEnv :: Parser WebAppEnv
pWebEnv = (\port -> defaultWebAppEnv {eWebPort = port}) <$> pWarpPort

pLlmEnv :: Parser LlmEnv
pLlmEnv = do
  apiKey <-
    setting
      [ help "Gemini API key for LLM news monitoring",
        reader str,
        env "GEMINI_API_KEY"
      ]
  modelName <-
    setting
      [ help "Gemini model name",
        reader str,
        env "GEMINI_MODEL"
      ]
  return LlmEnv {..}

pConfig :: Parser Config
pConfig = Config <$> pWebEnv <*> pPGSConnectInfo <*> pWorkerInterval <*> pLlmEnv

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- For debugging
  hSetBuffering stderr LineBuffering

  (Config webEnv pgConnectInfo workerInterval llmEnv) <-
    runParser (makeVersion []) "News notification service" pConfig
  dbPool <- createConnPool pgConnectInfo
  let env = Env dbPool workerInterval llmEnv

  void $ forkIO $ void $ runRuleWorker env
  runServer webEnv env
