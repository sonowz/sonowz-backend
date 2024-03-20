module Sonowz.StockNoti.Notification
  ( createNotification,
    StockNotificationType (..),
  )
where

import Control.Exception.Safe qualified as E
import Data.Time (Day)
import Database.PostgreSQL.Simple (Connection)
import Sonowz.Core.DB.CRUD (CRUDQueries (..))
import Sonowz.Core.DB.Pool (DBEffects, withDBConn)
import Sonowz.Core.DB.Utils (DatabaseException (..))
import Sonowz.Noti.Notification.DB.Queries (insertNotification)
import Sonowz.Noti.Notification.Types
  ( Notification (..),
    NotificationBody (TextBody),
    NotificationType (Email),
  )
import Sonowz.StockNoti.Imports
import Sonowz.StockNoti.Notification.Record.DB.Queries (findStockNotiRecord, stockNotiRecordCRUD)
import Sonowz.StockNoti.Notification.Record.DB.Types (StockNotiRecord' (..))
import Sonowz.StockNoti.Notification.Types (StockNotificationType (..))
import Sonowz.StockNoti.Stock.Types (StockSymbol)

createNotification :: (Members DBEffects r, HasCallStack) => StockSymbol -> StockNotificationType -> Day -> Sem r (Maybe Notification)
createNotification stockSymbol notiType timestamp = withDBConn $ \conn -> do
  isAlreadyNotified <- checkAndInsertNotiRecord conn stockSymbol notiType timestamp
  if isAlreadyNotified
    then do
      logDebug $ show stockSymbol <> " at " <> show timestamp <> " was already notified."
      return Nothing
    else do
      logInfo $ "Creating notification for " <> show stockSymbol <> " in " <> show timestamp <> "..."
      maybeCreated <- liftIO $ insertNotification conn (makeNoti stockSymbol notiType timestamp)
      case maybeCreated of
        Just noti -> return $ Just noti
        Nothing -> liftIO $ E.throw (DatabaseException "Could not insert notification!")

makeNoti :: StockSymbol -> StockNotificationType -> Day -> Notification
makeNoti stockSymbol notiType timestamp = Notification Email title body Nothing
  where
    title = "[Stock Notification] " <> show stockSymbol <> " " <> msgByNotiType <> "!"
    msgByNotiType = case notiType of
      NotiGoldenCross -> "Golden Cross"
      NotiDeadCross -> "Dead Cross"
    body = TextBody $ "Notification timestamp: " <> show timestamp

checkAndInsertNotiRecord :: Members DBEffects r => Connection -> StockSymbol -> StockNotificationType -> Day -> Sem r Bool
checkAndInsertNotiRecord conn stockSymbol notiType timestamp = do
  maybeRecord <- liftIO $ findStockNotiRecord conn stockSymbol notiType timestamp
  case maybeRecord of
    Just _ -> return True
    Nothing -> do
      let newRecord = StockNotiRecord' Nothing stockSymbol notiType timestamp
      liftIO $ crudCreate stockNotiRecordCRUD conn newRecord
      return False
