module Main where

import MCP.Server
  ( McpServerHandlers (McpServerHandlers),
    McpServerInfo (..),
    runMcpServerStdio,
  )
import Network.Wai.Handler.Warp (Port)
import Options.Applicative
import Sonowz.Core.Options.Applicative.Common (pWarpPort)
import Sonowz.Core.Web.Warp (runAppWithAccessLog)
import Sonowz.McpServers.Imports

data Config = Config Port

pConfig :: Parser Config
pConfig = Config <$> pWarpPort

opts :: ParserInfo Config
opts = info (helper <*> pConfig) (fullDesc <> progDesc "Sonowz MCP Servers")

mcpServerInfo :: McpServerInfo
mcpServerInfo =
  McpServerInfo
    { serverName = "Sonowz MCP Servers",
      serverVersion = "1.0.0",
      serverInstructions = "Set of tools"
    }

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- For debugging
  hSetBuffering stderr LineBuffering

  (Config warpPort) <- execParser opts

  runMcpServerStdio mcpServerInfo (McpServerHandlers Nothing Nothing Nothing)
