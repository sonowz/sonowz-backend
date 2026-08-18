module Sonowz.NewsCombinator.Rule.DB.Queries
  ( newsScrapRuleCRUD,
    getNewsScrapRules,
    updateNewsScrapRule,
  )
where

import Control.Exception.Safe qualified as E
import Data.Profunctor (dimap)
import Database.PostgreSQL.Simple (Connection)
import Opaleye
import Sonowz.Core.DB.CRUD (CRUDQueries (..), getCRUDQueries)
import Sonowz.Core.DB.Field (Uid)
import Sonowz.Core.DB.Utils (DatabaseException (DatabaseException))
import Sonowz.NewsCombinator.Imports
import Sonowz.NewsCombinator.Rule.DB.Types
import Sonowz.NewsCombinator.Rule.Types (NewsScrapRule (..))
import Sonowz.NewsCombinator.Rule.Types qualified as Rule (NewsScrapRule (uid))

-- Table declarations --

{-
CREATE TABLE public.news_scrap_rule (
    uid serial PRIMARY KEY NOT NULL,
    description text NOT NULL,
    confidence_level text NOT NULL,
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
          description = tableField "description",
          confidenceLevel = tableField "confidence_level",
          isEnabled = tableField "is_enabled",
          isOneTimeRule = tableField "is_one_time_rule",
          createdTime = tableField "created_time",
          updatedTime = tableField "updated_time"
        }

-- Public Interfaces --

-- This is raw-type interface used in web module
newsScrapRuleCRUD :: CRUDQueries Uid NewsScrapRule NewsScrapRule
newsScrapRuleCRUD = dimap toWriteDto fromDto $ getCRUDQueries newsScrapRuleTable

getNewsScrapRules :: Connection -> IO [NewsScrapRule]
getNewsScrapRules = crudList newsScrapRuleCRUD

updateNewsScrapRule :: Connection -> NewsScrapRule -> IO ()
updateNewsScrapRule conn rule =
  toDBException =<< do
    let Just _uid = Rule.uid rule
    crudUpdate newsScrapRuleCRUD conn _uid rule
  where
    toDBException :: Maybe a -> IO ()
    toDBException (Just _) = pass
    toDBException Nothing = E.throw (DatabaseException "Update failed!")

-- Private Functions --

fromDto :: NewsScrapRuleDto -> NewsScrapRule
fromDto NewsScrapRule' {..} =
  NewsScrapRule (Just uid) description confidenceLevel isEnabled isOneTimeRule

toWriteDto :: NewsScrapRule -> NewsScrapRuleWriteDto
toWriteDto NewsScrapRule {..} =
  NewsScrapRule'
    { uid = Nothing,
      description = description,
      confidenceLevel = confidenceLevel,
      isEnabled = isEnabled,
      isOneTimeRule = isOneTimeRule,
      createdTime = Nothing,
      updatedTime = Nothing
    }
