module Main where

import Data.Version (makeVersion)
import Database.PostgreSQL.Simple qualified as PGS
import OptEnvConf
import Sonowz.Core.Config.Common (pPGSConnectInfo)
import Sonowz.Core.DB.Pool (createConnPool)
import Sonowz.StockNoti.App (runApp)
import Sonowz.StockNoti.Env (Env (..))
import Sonowz.StockNoti.Imports
import Sonowz.StockNoti.Stock.Types (StockSymbol (..))

data Config = Config [StockSymbol] PGS.ConnectInfo Int

pWorkerInterval :: Parser Int
pWorkerInterval =
  setting
    [ help "Time interval between worker runs (in seconds)",
      reader auto,
      long "worker-interval",
      short 't',
      option,
      metavar "SECONDS",
      value 7200
    ]

pCrossSymbols :: Parser [StockSymbol]
pCrossSymbols = StockSymbol . toText @String <<$>> some symbols
  where
    symbols =
      setting
        [ help "List of stock symbols",
          reader str,
          argument,
          metavar "SYMBOLS..."
        ]

pConfig :: Parser Config
pConfig = Config <$> pCrossSymbols <*> pPGSConnectInfo <*> pWorkerInterval

main :: IO Void
main = do
  hSetBuffering stdout LineBuffering -- For debugging
  hSetBuffering stderr LineBuffering

  setStdLogActionLevel Info

  (Config stockSymbols pgConnectInfo workerInterval) <-
    runParser (makeVersion []) "Stock notification service" pConfig

  dbPool <- createConnPool pgConnectInfo
  let env = Env dbPool workerInterval stockSymbols

  runApp env
