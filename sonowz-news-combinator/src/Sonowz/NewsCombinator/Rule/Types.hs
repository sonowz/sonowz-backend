module Sonowz.NewsCombinator.Rule.Types
  ( NewsScrapRule (..),
    oneTimeRule,
    permanentRule,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (NominalDiffTime, nominalDay)
import Sonowz.Core.DB.Field (Uid)
import Sonowz.NewsCombinator.Imports

data NewsScrapRule = NewsScrapRule
  { uid :: Maybe Uid,
    keyword :: Text,
    successCount :: Int,
    successPeriod :: NominalDiffTime,
    isEnabled :: Bool,
    isOneTimeRule :: Bool
  }
  deriving (Show, Generic)

instance ToJSON NewsScrapRule

instance FromJSON NewsScrapRule

oneTimeRule :: Text -> Int -> NewsScrapRule
oneTimeRule keyword successCount = NewsScrapRule {..}
  where
    (uid, successPeriod, isEnabled, isOneTimeRule) = (Nothing, nominalDay, True, True)

permanentRule :: Text -> Int -> NewsScrapRule
permanentRule keyword successCount = NewsScrapRule {..}
  where
    (uid, successPeriod, isEnabled, isOneTimeRule) = (Nothing, nominalDay, True, False)
