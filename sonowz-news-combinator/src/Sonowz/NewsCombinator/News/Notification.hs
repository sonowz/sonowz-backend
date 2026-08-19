module Sonowz.NewsCombinator.News.Notification
  ( createNotification,
  )
where

import Control.Exception.Safe qualified as E
import Sonowz.Core.DB.Pool (DBEffects, withDBConn)
import Sonowz.Core.DB.Utils (DatabaseException (..))
import Sonowz.NewsCombinator.Imports
import Sonowz.NewsCombinator.News.Types (LlmEvaluationResult (..), NewsArticle (..))
import Sonowz.NewsCombinator.Rule.Types (NewsScrapRule (..))
import Sonowz.Noti.Notification.DB.Queries (insertNotification)
import Sonowz.Noti.Notification.Types
  ( Notification (..),
    NotificationBody (HTMLBody),
    NotificationType (Email),
  )

createNotification :: (Members DBEffects r) => NewsScrapRule -> LlmEvaluationResult -> Sem r Notification
createNotification rule evalResult = withDBConn $ \conn -> do
  maybeCreated <- liftIO $ insertNotification conn (makeNoti rule evalResult)
  case maybeCreated of
    Just noti -> return noti
    Nothing -> liftIO $ E.throw (DatabaseException "Could not insert notification!")

makeNoti :: NewsScrapRule -> LlmEvaluationResult -> Notification
makeNoti rule evalResult = Notification Email notiTitle body Nothing
  where
    notiTitle =
      if isOneTimeRule rule
        then "[News Combinator] Match found for: \"" <> description rule <> "\""
        else "[News Combinator] News update: \"" <> description rule <> "\""
    body =
      HTMLBody ("<p>" <> summary evalResult <> "</p>")
        <> if null (matchedArticles evalResult)
          then HTMLBody ""
          else HTMLBody "<ul>" <> foldMap articleToBody (matchedArticles evalResult) <> HTMLBody "</ul>"
    articleToBody :: NewsArticle -> NotificationBody
    articleToBody NewsArticle {..} =
      HTMLBody $ "<li><a href=\"" <> link <> "\">" <> title <> "</a> (" <> show publishedAt <> ")</li>"
