{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoStrictData #-}

module Sonowz.NewsCombinator.Rule.DB.Types where

import Data.Profunctor.Product.Default (Default (..))
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Time (UTCTime)
import Opaleye
import Sonowz.Core.DB.Entity (Entity (..))
import Sonowz.Core.DB.Field (EmptyField, Uid)
import Sonowz.Core.DB.Utils (fieldParserByReadInstance)
import Sonowz.NewsCombinator.Imports
import Sonowz.NewsCombinator.Rule.Types (ConfidenceLevel (..))

data NewsScrapRule' c1 c2 c3 c4 c5 c6 c7 = NewsScrapRule'
  { uid :: c1,
    description :: c2,
    confidenceLevel :: c3,
    isEnabled :: c4,
    isOneTimeRule :: c5,
    createdTime :: c6,
    updatedTime :: c7
  }
  deriving (Show, Generic)

type NewsScrapRuleWriteDto = NewsScrapRule' EmptyField Text ConfidenceLevel Bool Bool EmptyField EmptyField

type NewsScrapRuleDto = NewsScrapRule' Uid Text ConfidenceLevel Bool Bool UTCTime UTCTime

type NewsScrapRuleW =
  NewsScrapRule'
    (Maybe (Field SqlInt4))
    (Field SqlText)
    (Field SqlText)
    (Field SqlBool)
    (Field SqlBool)
    (Maybe (Field SqlTimestamptz))
    (Maybe (Field SqlTimestamptz))

type NewsScrapRuleR =
  NewsScrapRule'
    (Field SqlInt4)
    (Field SqlText)
    (Field SqlText)
    (Field SqlBool)
    (Field SqlBool)
    (Field SqlTimestamptz)
    (Field SqlTimestamptz)

instance Entity NewsScrapRuleR where
  entityIdField = uid
  entityToFields _ = toFields

type NewsScrapRuleTable = Table NewsScrapRuleW NewsScrapRuleR

-- Opaleye-related stuffs --
$(makeAdaptorAndInstance "pNewsScrapRule" ''NewsScrapRule')

instance DefaultFromField SqlText ConfidenceLevel where
  defaultFromField = fromPGSFieldParser $ fieldParserByReadInstance "ConfidenceLevel"

instance Default ToFields ConfidenceLevel (Field SqlText) where
  def = toToFields (sqlStrictText . show)
