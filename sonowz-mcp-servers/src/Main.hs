module Main where

import Network.Wai.Handler.Warp (Port)
import Options.Applicative
import Sonowz.Core.DB.Pool (createConnPool)
import Sonowz.Core.Options.Applicative.Common (pPGSConnectInfo, pWarpPort)
import Sonowz.Core.Web.Warp (runAppWithAccessLog)
import Sonowz.McpServers.Imports

data Config = Config Port

pConfig :: Parser Config
pConfig = Config <$> pWarpPort

opts :: ParserInfo Config
opts = info (helper <*> pConfig) (fullDesc <> progDesc "Sonowz RAG API server")

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- For debugging
  hSetBuffering stderr LineBuffering

  (Config warpPort) <- execParser opts
  pass
