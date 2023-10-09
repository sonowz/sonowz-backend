module Sonowz.StockNoti.Env
  ( Env (..),
  )
where

import Sonowz.Core.DB.Pool (DBConnPool)
import Sonowz.StockNoti.Imports
import Sonowz.StockNoti.Stock.Types (StockSymbol)

data Env = Env
  { envPgConnection :: DBConnPool,
    envWorkerIntervalSeconds :: Int,
    envStockSymbols :: [StockSymbol]
  }
