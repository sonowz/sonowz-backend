module Sonowz.Rag.Env
  ( Env (..),
  )
where

import Sonowz.Core.DB.Pool (DBConnPool)
import Sonowz.Rag.Imports

data Env = Env
  { envPgConnection :: DBConnPool,
    envOpenAIKey :: Text
  }
