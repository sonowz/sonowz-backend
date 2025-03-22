module Sonowz.Rag.Embedding.Types
  ( OpenAIKey (..),
  )
where

import Sonowz.Rag.Imports

newtype OpenAIKey = OpenAIKey {getKey :: Text}