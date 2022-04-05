module Sonowz.NewsCombinator.News.Notification
  ( createNotification
  ) where

import Sonowz.Core.DB.Pool (DBEffects, withDBConn)
import Sonowz.Core.DB.Utils (DatabaseException(..))
import Sonowz.NewsCombinator.Imports
import Sonowz.NewsCombinator.News.Types (NewsItem(..))
import Sonowz.NewsCombinator.Rule.Types (NewsScrapRule(..))
import Sonowz.Noti.Notification.DB.Queries (insertNotification)
import Sonowz.Noti.Notification.Types
  (Notification(..), NotificationBody(HTMLBody), NotificationType(Email))


createNotification :: Members DBEffects r => NewsScrapRule -> [NewsItem] -> Sem r Notification
createNotification rule newsItems = withDBConn $ \conn -> do
  maybeCreated <- liftIO $ insertNotification conn (makeNoti rule newsItems)
  case maybeCreated of
    Just noti -> return noti
    Nothing   -> throw' (DatabaseException "Could not insert notification!")

makeNoti :: NewsScrapRule -> [NewsItem] -> Notification
makeNoti rule items = Notification Email title body Nothing where
  title = if isOneTimeRule rule
    then "[News Combinator] \"" <> keyword rule <> "\" appeared!"
    else "[News Combinator] \"" <> keyword rule <> "\" news!"
  body = HTMLBody "<ul>" <> fold (newsToBody <$> items) <> HTMLBody "</ul>"
  newsToBody :: NewsItem -> NotificationBody
  newsToBody news = HTMLBody $ "<li>" <> getTitle news <> " (" <> show (getDate news) <> ")</li>"

