{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoStrictData #-}

module Sonowz.Rag.Document.DB.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Time (UTCTime)
import Opaleye
import Sonowz.Core.DB.Entity (Entity (..))
import Sonowz.Core.DB.Field (EmptyField, Uid)
import Sonowz.Rag.Imports

data RawDocument' c1 c2 c3 c4 c5 c6 c7 = RawDocument'
  { uid :: c1,
    documentId :: c2,
    source :: c3,
    title :: c4,
    document :: c5,
    createdTime :: c6,
    updatedTime :: c7
  }
  deriving (Generic)

type RawDocumentWriteDto = RawDocument' EmptyField Text Text Text Text EmptyField EmptyField

instance FromJSON RawDocumentWriteDto

type RawDocument = RawDocument' Uid Text Text Text Text UTCTime UTCTime

instance ToJSON RawDocument

type RawDocumentFieldW =
  RawDocument' -- Write fields
    (Maybe (Field SqlInt4))
    (Field SqlText)
    (Field SqlText)
    (Field SqlText)
    (Field SqlText)
    (Maybe (Field SqlTimestamptz))
    (Maybe (Field SqlTimestamptz))

type RawDocumentFieldR =
  RawDocument' -- Read fields
    (Field SqlInt4)
    (Field SqlText)
    (Field SqlText)
    (Field SqlText)
    (Field SqlText)
    (Field SqlTimestamptz)
    (Field SqlTimestamptz)

instance Entity RawDocumentFieldR where
  entityIdField = uid
  entityToFields _ = toFields

type RawDocumentTable = Table RawDocumentFieldW RawDocumentFieldR

$(makeAdaptorAndInstance "pRawDocument" ''RawDocument')
