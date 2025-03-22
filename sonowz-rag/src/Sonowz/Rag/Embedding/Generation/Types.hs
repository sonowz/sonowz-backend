module Sonowz.Rag.Embedding.Generation.Types
  ( OpenAIKey (..),
  )
where

import Sonowz.Rag.Imports

newtype OpenAIKey = OpenAIKey {getKey :: Text}