module Sonowz.Rag.Document.Types
  ( RawDocument (..),
  )
where

import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (FromRow)
import Sonowz.Core.DB.Field (Uid)
import Sonowz.Rag.Imports

data RawDocument = RawDocument
  { uid :: Uid,
    documentId :: Text,
    source :: Text,
    title :: Text,
    document :: Text,
    createdTime :: UTCTime,
    updatedTime :: UTCTime
  }
  deriving (Show, Generic, FromRow)