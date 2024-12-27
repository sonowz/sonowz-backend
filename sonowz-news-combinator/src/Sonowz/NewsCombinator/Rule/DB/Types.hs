{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoStrictData #-}

module Sonowz.NewsCombinator.Rule.DB.Types where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Time (NominalDiffTime, UTCTime)
import Opaleye
import Sonowz.Core.DB.Entity (Entity (..))
import Sonowz.Core.DB.Field (EmptyField, Uid)
import Sonowz.NewsCombinator.Imports

data NewsScrapRule' c1 c2 c3 c4 c5 c6 c7 c8 = NewsScrapRule'
  { uid :: c1,
    keyword :: c2,
    successCount :: c3,
    successPeriod :: c4,
    isEnabled :: c5,
    isOneTimeRule :: c6,
    createdTime :: c7,
    updatedTime :: c8
  }
  deriving (Show, Generic)

type NewsScrapRuleWriteDto = NewsScrapRule' EmptyField Text Int NominalDiffTime Bool Bool EmptyField EmptyField

type NewsScrapRuleDto = NewsScrapRule' Uid Text Int NominalDiffTime Bool Bool UTCTime UTCTime

type NewsScrapRuleW =
  NewsScrapRule'
    (Maybe (Field SqlInt4))
    (Field SqlText)
    (Field SqlInt4)
    (Field SqlFloat8)
    (Field SqlBool)
    (Field SqlBool)
    (Maybe (Field SqlTimestamptz))
    (Maybe (Field SqlTimestamptz))

type NewsScrapRuleR =
  NewsScrapRule'
    (Field SqlInt4)
    (Field SqlText)
    (Field SqlInt4)
    (Field SqlFloat8)
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
