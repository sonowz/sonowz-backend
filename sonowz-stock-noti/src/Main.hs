module Main where

import Database.PostgreSQL.Simple qualified as PGS
import Options.Applicative
import Sonowz.Core.DB.Pool (createConnPool)
import Sonowz.Core.Options.Applicative.Common (pPGSConnectInfo)
import Sonowz.StockNoti.App (runApp)
import Sonowz.StockNoti.Env (Env (..))
import Sonowz.StockNoti.Imports
import Sonowz.StockNoti.Stock.Types (StockSymbol (..))

data Config = Config [StockSymbol] PGS.ConnectInfo Int

pWorkerInterval :: Parser Int
pWorkerInterval = option auto (long "worker-interval" <> short 't' <> metavar "SECONDS" <> value 7200 <> help helpMsg)
  where
    helpMsg = "Time interval between worker runs (in seconds)"

-- TODO: make symbols configurable via web
{-
pWebEnv :: Parser WebAppEnv
pWebEnv = do
  port <- option (auto >>= checkPort) (long "port" <> short 'p' <> value 80)
  return $ defaultWebAppEnv {eWebPort = port}
  where
    checkPort port = if 0 < port && port < 90000 then return port else empty
-}

pCrossSymbols :: Parser [StockSymbol]
pCrossSymbols = StockSymbol . toText @String <<$>> some (strArgument (metavar "SYMBOLS..." <> help helpMsg))
  where
    helpMsg = "List of stock symbols"

pConfig :: Parser Config
pConfig = Config <$> pCrossSymbols <*> pPGSConnectInfo <*> pWorkerInterval

opts :: ParserInfo Config
opts = info (helper <*> pConfig) (fullDesc <> progDesc "Stock notification service")

main :: IO Void
main = do
  hSetBuffering stdout LineBuffering -- For debugging
  hSetBuffering stderr LineBuffering

  setStdLogActionLevel Info

  (Config stockSymbols pgConnectInfo workerInterval) <- execParser opts
  dbPool <- createConnPool pgConnectInfo
  let env = Env dbPool workerInterval stockSymbols

  runApp env
