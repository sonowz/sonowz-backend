module Sonowz.NewsCombinator.Env
  ( Env (..),
  )
where

import Sonowz.Core.DB.Pool (DBConnPool)
import Sonowz.NewsCombinator.Imports

data Env = Env
  { envPgConnection :: DBConnPool,
    envWorkerIntervalSeconds :: Int
  }
