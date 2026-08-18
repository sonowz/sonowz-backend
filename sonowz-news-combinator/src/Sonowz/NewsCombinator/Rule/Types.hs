module Sonowz.NewsCombinator.Rule.Types
  ( ConfidenceLevel (..),
    NewsScrapRule (..),
    oneTimeRule,
    permanentRule,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Sonowz.Core.DB.Field (Uid)
import Sonowz.NewsCombinator.Imports

data ConfidenceLevel = Rumor | Official
  deriving (Show, Read, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data NewsScrapRule = NewsScrapRule
  { uid :: Maybe Uid,
    description :: Text,
    confidenceLevel :: ConfidenceLevel,
    isEnabled :: Bool,
    isOneTimeRule :: Bool
  }
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

oneTimeRule :: Text -> ConfidenceLevel -> NewsScrapRule
oneTimeRule description confidenceLevel = NewsScrapRule {..}
  where
    (uid, isEnabled, isOneTimeRule) = (Nothing, True, True)

permanentRule :: Text -> ConfidenceLevel -> NewsScrapRule
permanentRule description confidenceLevel = NewsScrapRule {..}
  where
    (uid, isEnabled, isOneTimeRule) = (Nothing, True, False)
