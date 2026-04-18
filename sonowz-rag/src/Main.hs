module Main where

import Data.Version (makeVersion)
import Database.PostgreSQL.Simple qualified as PGS
import Network.Wai.Handler.Warp (Port)
import OptEnvConf
import Servant
import Sonowz.Core.Config.Common (pPGSConnectInfo, pWarpPort)
import Sonowz.Core.DB.Pool (createConnPool)
import Sonowz.Core.Web.Warp (runAppWithAccessLog)
import Sonowz.Rag.App (RagServerAPI, runWithEffects, server)
import Sonowz.Rag.Embedding.Types (OpenAIKey (..))
import Sonowz.Rag.Env (Env (..))
import Sonowz.Rag.Imports

data Config = Config Port PGS.ConnectInfo Text

pOpenAIKey :: Parser Text
pOpenAIKey =
  setting
    [ help "OpenAI API key",
      reader str,
      long "openai"
    ]

pConfig :: Parser Config
pConfig = Config <$> pWarpPort <*> pPGSConnectInfo <*> pOpenAIKey

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- For debugging
  hSetBuffering stderr LineBuffering

  (Config warpPort pgConnectInfo openAIKey) <-
    runParser (makeVersion []) "Sonowz RAG API server" pConfig
  dbPool <- createConnPool pgConnectInfo
  let env = Env {envPgConnection = dbPool, envOpenAIKey = OpenAIKey openAIKey}

  let waiApp = serve api (hoistServer api nt server)
      api = Proxy :: Proxy RagServerAPI
      nt :: forall x. Sem _ x -> Handler x -- Natural Transformation from 'Sem r' to 'Handler'
      nt = runWithEffects env
  runAppWithAccessLog warpPort waiApp
