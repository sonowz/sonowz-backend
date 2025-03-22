module Sonowz.Rag.Rag.Types
  ( RagResultDocument (..),
  )
where

import Data.Aeson (ToJSON)
import Sonowz.Rag.Imports

data RagResultDocument = RagResultDocument
  { title :: Text,
    document :: Text
  }
  deriving (Generic, Show, ToJSON)