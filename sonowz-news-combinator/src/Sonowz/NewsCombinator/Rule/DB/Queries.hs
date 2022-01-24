module Sonowz.NewsCombinator.Rule.DB.Queries
  ( newsScrapRuleCRUD
  , getNewsScrapRules
  , updateNewsScrapRule
  ) where

import qualified Control.Exception.Safe as E
import Database.PostgreSQL.Simple (Connection)
import Opaleye
import Relude (hoistMaybe)
import Sonowz.Core.DB.CRUD (CRUDQueries(..), getCRUDQueries)
import Sonowz.Core.DB.Utils (DatabaseException(DatabaseException))
import Sonowz.NewsCombinator.Imports
import Sonowz.NewsCombinator.Rule.DB.Types
import Sonowz.NewsCombinator.Rule.Types (NewsScrapRule(..))
import Sonowz.Noti.Notification.Types (Uid)


-- Table declarations --

newsScrapRuleTable :: NewsScrapRuleTable
newsScrapRuleTable = table "news_scrap_rule_table" (pNewsScrapRule fields) where
  fields = NewsScrapRule'
    { uid           = tableField "uid"
    , keyword       = tableField "keyword"
    , successCount  = tableField "success_count"
    , successPeriod = tableField "success_period"
    , isEnabled     = tableField "is_enabled"
    , isOneTimeRule = tableField "is_one_time_rule"
    , createdTime   = tableField "created_time"
    , updatedTime   = tableField "updated_time"
    }

-- Public Interfaces --

-- This is raw-type interface used in web module
newsScrapRuleCRUD :: CRUDQueries NewsScrapRuleHask NewsScrapRuleHaskW Uid
newsScrapRuleCRUD = getCRUDQueries newsScrapRuleTable Sonowz.NewsCombinator.Rule.DB.Types.uid

getNewsScrapRules :: Connection -> IO [NewsScrapRule]
getNewsScrapRules = fmap (fmap haskToRule) . crudList newsScrapRuleCRUD

updateNewsScrapRule :: Connection -> NewsScrapRule -> IO ()
updateNewsScrapRule conn rule = toDBException =<< runMaybeT
  (do
    (uid, hask) <- hoistMaybe $ ruleToUidAndHask rule
    MaybeT $ crudUpdate newsScrapRuleCRUD conn uid hask
  ) where
  toDBException :: Maybe a -> IO ()
  toDBException (Just _) = pass
  toDBException Nothing  = E.throw (DatabaseException "Update failed!")

-- Private Functions --

haskToRule :: NewsScrapRuleHask -> NewsScrapRule
haskToRule NewsScrapRule' {..} =
  NewsScrapRule (Just uid) keyword successCount successPeriod isEnabled isOneTimeRule

ruleToUidAndHask :: NewsScrapRule -> Maybe (Uid, NewsScrapRuleHaskW)
ruleToUidAndHask NewsScrapRule {..} = do
  let Just _uid = uid
  return
    ( _uid
    , NewsScrapRule'
      { uid           = error "Unexpected 'uid' access"
      , keyword       = keyword
      , successCount  = successCount
      , successPeriod = successPeriod
      , isEnabled     = isEnabled
      , isOneTimeRule = isOneTimeRule
      , createdTime   = error "Unexpected 'createdTime' access"
      , updatedTime   = error "Unexpected 'updatedTime' access"
      }
    )
