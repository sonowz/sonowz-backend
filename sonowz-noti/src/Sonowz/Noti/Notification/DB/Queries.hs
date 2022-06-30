module Sonowz.Noti.Notification.DB.Queries
  ( insertNotification,
    selectOneNotification,
    deleteNotificationByUid,
  )
where

import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Transaction (withTransaction)
import Opaleye
import Sonowz.Core.DB.CRUD (CRUDQueries (crudCreate, crudDelete), getCRUDQueries)
import Sonowz.Core.DB.Utils (Uid)
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
insertNotification conn noti = haskToNoti <<$>> crudCreate crudSet conn (notiToHaskW noti)

selectOneNotification :: HasCallStack => Connection -> IO (Maybe Notification)
selectOneNotification conn = withTransaction conn $ listToMaybe <$> (haskToNoti <<$>> selectResult)
  where
    selectResult = runSelect conn (qSelectMinNotification notificationTable)

deleteNotificationByUid :: HasCallStack => Connection -> Uid -> IO Bool
deleteNotificationByUid = crudDelete crudSet

-- Private Functions --

crudSet :: CRUDQueries NotificationHask NotificationHaskW Uid
crudSet = getCRUDQueries notificationTable uid

haskToNoti :: NotificationHask -> Notification
haskToNoti Notification' {..} = Notification _type title body (Just uid)

notiToHaskW :: Notification -> NotificationHaskW
notiToHaskW Notification {..} =
  Notification'
    { uid = error "Unexpected 'uid' access",
      _type = notificationType,
      title = notificationTitle,
      body = notificationBody,
      createdTime = error "Unexpected 'createdTime' access"
    }

-- Queries --

qSelectMinNotification :: NotificationTable -> Select NotificationFieldR
qSelectMinNotification table = limit 1 $ orderBy (asc uid) $ selectTable table
