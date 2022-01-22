module Sonowz.NewsCombinator.Rule.Types
  ( NewsScrapRule(..)
  , oneTimeRule
  , permanentRule
  ) where

import Data.Time (NominalDiffTime, nominalDay)
import Sonowz.NewsCombinator.Imports
import Sonowz.Noti.Notification.Types (Uid)

data NewsScrapRule = NewsScrapRule
  { uid           :: Maybe Uid
  , keyword       :: Text
  , successCount  :: Int
  , successPeriod :: NominalDiffTime
  , isEnabled     :: Bool
  , isOneTimeRule :: Bool
  }
  deriving Show

oneTimeRule :: Text -> Int -> NewsScrapRule
oneTimeRule keyword successCount = NewsScrapRule { .. }
  where (uid, successPeriod, isEnabled, isOneTimeRule) = (Nothing, nominalDay, True, True)

permanentRule :: Text -> Int -> NewsScrapRule
permanentRule keyword successCount = NewsScrapRule { .. }
  where (uid, successPeriod, isEnabled, isOneTimeRule) = (Nothing, nominalDay, True, False)
