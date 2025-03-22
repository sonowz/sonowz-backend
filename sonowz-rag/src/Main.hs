module Main where

import Database.PostgreSQL.Simple qualified as PGS
import Network.Wai.Handler.Warp (Port)
import Options.Applicative
import Servant
import Sonowz.Core.DB.Pool (createConnPool)
import Sonowz.Core.Options.Applicative.Common (pPGSConnectInfo, pWarpPort)
import Sonowz.Core.Web.Warp (runAppWithAccessLog)
import Sonowz.Rag.App (RagServerAPI, runWithEffects, server)
import Sonowz.Rag.Embedding.Types (OpenAIKey (..))
import Sonowz.Rag.Env (Env (..))
import Sonowz.Rag.Imports

data Config = Config Port PGS.ConnectInfo Text

pOpenAIKey :: Parser Text
pOpenAIKey = strOption (long "openai")

pConfig :: Parser Config
pConfig = Config <$> pWarpPort <*> pPGSConnectInfo <*> pOpenAIKey

opts :: ParserInfo Config
opts = info (helper <*> pConfig) (fullDesc <> progDesc "Sonowz RAG API server")

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- For debugging
  hSetBuffering stderr LineBuffering

  (Config warpPort pgConnectInfo openAIKey) <- execParser opts
  dbPool <- createConnPool pgConnectInfo
  let env = Env {envPgConnection = dbPool, envOpenAIKey = OpenAIKey openAIKey}

  let waiApp = serve api (hoistServer api nt server)
      api = Proxy :: Proxy RagServerAPI
      nt :: forall x. Sem _ x -> Handler x -- Natural Transformation from 'Sem r' to 'Handler'
      nt = runWithEffects env
  runAppWithAccessLog warpPort waiApp
