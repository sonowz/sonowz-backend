module Sonowz.NewsCombinator.Rule.Executor
  ( evalNewsScrapRule,
  )
where

import Data.Time (UTCTime, addUTCTime, zonedTimeToUTC)
import Sonowz.Core.Exception.Types (ParseException (ParseException))
import Sonowz.Core.HTTP.Effect (HTTP, fetchURL)
import Sonowz.Core.Time.Effect (Time, getTime)
import Sonowz.NewsCombinator.Imports
import Sonowz.NewsCombinator.News.Parser (parseNewsItems)
import Sonowz.NewsCombinator.News.Types (NewsItem (..), googleNewsRSSUrl)
import Sonowz.NewsCombinator.Rule.Types (NewsScrapRule (..))

-- Scrap news, then decide whether success or not
-- If success, rule could be modified
evalNewsScrapRule ::
  (Members '[Time, HTTP, Error ParseException] r, Members StdEff r) =>
  NewsScrapRule ->
  Sem r (Maybe [NewsItem], NewsScrapRule)
evalNewsScrapRule rule = do
  assertRuleEnabled
  let requestURL = googleNewsRSSUrl (keyword rule)
  body <- fetchURL requestURL
  parsedItems <- fromEither $ parseNewsItems body
  (now :: UTCTime) <- zonedTimeToUTC <$> getTime
  case checkNewsRule parsedItems rule now of
    Just newsItems -> return (Just newsItems, whenSuccess rule)
    Nothing -> return (Nothing, whenFail rule)
  where
    assertRuleEnabled =
      if not (isEnabled rule)
        then
          let e = ParseException "Rule is disabled!"
           in logException e >> throw e
        else pass
    whenSuccess rule
      | isOneTimeRule rule = rule {isEnabled = False}
      | otherwise = rule
    whenFail = id

checkNewsRule :: [NewsItem] -> NewsScrapRule -> UTCTime -> Maybe [NewsItem]
checkNewsRule items rule now = do
  let recentItems = filter (\item -> getDate item > baseTime) items
      baseTime = addUTCTime (-(successPeriod rule)) now
  guard (length recentItems >= successCount rule)
  return recentItems
