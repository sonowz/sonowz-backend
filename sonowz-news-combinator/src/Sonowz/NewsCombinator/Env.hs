module Sonowz.NewsCombinator.Env
  ( Env (..),
  )
where

import Sonowz.Core.DB.Pool (DBConnPool)
import Sonowz.Core.Llm.Effect (LlmEnv (..))
import Sonowz.NewsCombinator.Imports

data Env = Env
  { pgConnection :: DBConnPool,
    workerIntervalSeconds :: Int,
    llmEnv :: LlmEnv
  }
