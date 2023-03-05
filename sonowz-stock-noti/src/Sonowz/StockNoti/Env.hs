module Sonowz.StockNoti.Env
  ( Env (..),
  )
where

import Sonowz.Core.DB.Pool (DBConnPool)
import Sonowz.StockNoti.Imports

data Env = Env
  { envPgConnection :: DBConnPool,
    envWorkerIntervalSeconds :: Int
  }
