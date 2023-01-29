module Sonowz.NewsCombinator.Rule.DB.Queries
  ( newsScrapRuleCRUD,
    getNewsScrapRules,
    updateNewsScrapRule,
  )
where

import Control.Exception.Safe qualified as E
import Database.PostgreSQL.Simple (Connection)
import Opaleye
import Sonowz.Core.DB.CRUD (CRUDQueries (..), getCRUDQueries)
import Sonowz.Core.DB.Field (Uid)
import Sonowz.Core.DB.Utils (DatabaseException (DatabaseException))
import Sonowz.NewsCombinator.Imports
import Sonowz.NewsCombinator.Rule.DB.Types
import Sonowz.NewsCombinator.Rule.Types (NewsScrapRule (..))

-- Table declarations --

{-
CREATE TABLE public.news_scrap_rule (
    uid serial PRIMARY KEY NOT NULL,
    keyword text NOT NULL,
    success_count integer NOT NULL,
    success_period double precision NOT NULL,
    is_enabled boolean NOT NULL,
    is_one_time_rule boolean NOT NULL,
    created_time timestamp with time zone DEFAULT now() NOT NULL,
    updated_time timestamp with time zone DEFAULT now() NOT NULL
);
CREATE TRIGGER news_scrap_rule_update BEFORE UPDATE ON public.news_scrap_rule FOR EACH ROW EXECUTE PROCEDURE public.update_time();
-}

newsScrapRuleTable :: NewsScrapRuleTable
newsScrapRuleTable = table "news_scrap_rule" (pNewsScrapRule fields)
  where
    fields =
      NewsScrapRule'
        { uid = tableField "uid",
          keyword = tableField "keyword",
          successCount = tableField "success_count",
          successPeriod = tableField "success_period",
          isEnabled = tableField "is_enabled",
          isOneTimeRule = tableField "is_one_time_rule",
          createdTime = tableField "created_time",
          updatedTime = tableField "updated_time"
        }

-- Public Interfaces --

-- This is raw-type interface used in web module
newsScrapRuleCRUD :: CRUDQueries NewsScrapRuleHask NewsScrapRuleHaskW Uid
newsScrapRuleCRUD = getCRUDQueries newsScrapRuleTable Sonowz.NewsCombinator.Rule.DB.Types.uid

getNewsScrapRules :: Connection -> IO [NewsScrapRule]
getNewsScrapRules = fmap (fmap haskToRule) . crudList newsScrapRuleCRUD

updateNewsScrapRule :: Connection -> NewsScrapRule -> IO ()
updateNewsScrapRule conn rule =
  toDBException
    =<< runMaybeT
      ( do
          (uid, hask) <- hoistMaybe $ ruleToUidAndHask rule
          MaybeT $ crudUpdate newsScrapRuleCRUD conn uid hask
      )
  where
    toDBException :: Maybe a -> IO ()
    toDBException (Just _) = pass
    toDBException Nothing = E.throw (DatabaseException "Update failed!")

-- Private Functions --

haskToRule :: NewsScrapRuleHask -> NewsScrapRule
haskToRule NewsScrapRule' {..} =
  NewsScrapRule (Just uid) keyword successCount successPeriod isEnabled isOneTimeRule

ruleToUidAndHask :: NewsScrapRule -> Maybe (Uid, NewsScrapRuleHaskW)
ruleToUidAndHask NewsScrapRule {..} = do
  let Just _uid = uid
  return
    ( _uid,
      NewsScrapRule'
        { uid = error "Unexpected 'uid' access",
          keyword = keyword,
          successCount = successCount,
          successPeriod = successPeriod,
          isEnabled = isEnabled,
          isOneTimeRule = isOneTimeRule,
          createdTime = error "Unexpected 'createdTime' access",
          updatedTime = error "Unexpected 'updatedTime' access"
        }
    )
