module Sonowz.Noti.DB.Queries
  ( insertNotification
  , selectOneNotification
  , deleteNotificationByUid
  ) where

import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Transaction (withTransaction)
import Opaleye

import Sonowz.Noti.DB.Types
import Sonowz.Noti.Imports
import Sonowz.Noti.Notification.Types (Notification(..), Uid)

-- Table declarations --

notificationTable :: NotificationTable
notificationTable = table "notification" (pNotification notificationFields)

notificationFields = Notification'
  { uid         = tableField "uid"
  , _type       = tableField "type"
  , title       = tableField "title"
  , body        = tableField "body"
  , createdTime = tableField "created_time"
  }

-- Public Interfaces --

insertNotification :: HasCallStack => Connection -> Notification -> IO Bool
insertNotification conn noti = withTransaction conn $ checkSuccess <$> insertResult where
  insertResult = runInsert_ conn (qInsertNotification notificationTable noti)
  checkSuccess 1 = True
  checkSuccess _ = False

selectOneNotification :: HasCallStack => Connection -> IO (Maybe Notification)
selectOneNotification conn = withTransaction conn $ listToMaybe <$> (construct <<$>> selectResult) where
  selectResult = runSelect conn (qSelectMinNotification notificationTable)
  construct :: NotificationHask -> Notification
  construct Notification' {..} = Notification _type title body (Just uid)

deleteNotificationByUid :: HasCallStack => Connection -> Uid -> IO Bool
deleteNotificationByUid conn uid = withTransaction conn $ checkSuccess <$> deleteResult where
  deleteResult = runDelete_ conn (qDeleteNotificationByUid notificationTable uid)
  checkSuccess 1 = True
  checkSuccess _ = False

-- Private Functions --

notiToWriteField :: Notification -> NotificationFieldW
notiToWriteField Notification {..} = Notification'
  { uid         = Nothing
  , _type       = toFields notificationType
  , title       = toFields notificationTitle
  , body        = toFields notificationBody
  , createdTime = Nothing
  }

-- Queries --

qInsertNotification :: NotificationTable -> Notification -> Insert Int64
qInsertNotification table noti = Insert
  { iTable      = table
  , iRows       = [notiToWriteField noti]
  , iReturning  = rCount
  , iOnConflict = Just DoNothing
  }

qSelectMinNotification :: NotificationTable -> Select NotificationFieldR
qSelectMinNotification table = limit 1 $ orderBy (asc uid) $ selectTable table

qDeleteNotificationByUid :: NotificationTable -> Uid -> Delete Int64
qDeleteNotificationByUid table targetUid =
  Delete { dTable = table, dWhere = \row -> uid row .== toFields targetUid, dReturning = rCount }
