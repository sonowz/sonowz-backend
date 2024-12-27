module Sonowz.Noti.Notification.DB.Queries
  ( insertNotification,
    selectOneNotification,
    deleteNotificationByUid,
  )
where

import Data.Profunctor (dimap)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Transaction (withTransaction)
import Opaleye
import Relude.Unsafe qualified as Unsafe
import Sonowz.Core.DB.CRUD (CRUDQueries (crudCreate, crudDelete), getCRUDQueries)
import Sonowz.Core.DB.Field (Uid)
import Sonowz.Noti.Imports
import Sonowz.Noti.Notification.DB.Types
import Sonowz.Noti.Notification.Types (Notification (..))

-- Table declarations --

{-
CREATE TABLE public.notification (
    uid serial PRIMARY KEY NOT NULL,
    type text NOT NULL,
    title text NOT NULL,
    body text NOT NULL,
    created_time timestamp with time zone DEFAULT now() NOT NULL
);
-}

notificationTable :: NotificationTable
notificationTable = table "notification" (pNotification notificationFields)

notificationFields =
  Notification'
    { uid = tableField "uid",
      _type = tableField "type",
      title = tableField "title",
      body = tableField "body",
      createdTime = tableField "created_time"
    }

-- Public Interfaces --

insertNotification :: HasCallStack => Connection -> Notification -> IO (Maybe Notification)
insertNotification = crudCreate crudSet

selectOneNotification :: HasCallStack => Connection -> IO (Maybe Notification)
selectOneNotification conn = withTransaction conn $ listToMaybe <$> (fromDto <<$>> selectResult)
  where
    selectResult = runSelect conn (qSelectMinNotification notificationTable)

deleteNotificationByUid :: HasCallStack => Connection -> Uid -> IO Bool
deleteNotificationByUid = crudDelete crudSet

-- Private Functions --

crudSet :: CRUDQueries Uid Notification Notification
crudSet = dimap toWriteDto fromDto $ getCRUDQueries notificationTable

fromDto :: NotificationDto -> Notification
fromDto Notification' {..} = Notification (Unsafe.read _type) title (Unsafe.read body) (Just uid)

toWriteDto :: Notification -> NotificationWriteDto
toWriteDto Notification {..} =
  Notification'
    { uid = Nothing,
      _type = show notificationType,
      title = notificationTitle,
      body = show notificationBody,
      createdTime = Nothing
    }

-- Queries --

qSelectMinNotification :: NotificationTable -> Select NotificationFieldR
qSelectMinNotification table = limit 1 $ orderBy (asc uid) $ selectTable table
