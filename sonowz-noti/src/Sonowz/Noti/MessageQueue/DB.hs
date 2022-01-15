module Sonowz.Noti.MessageQueue.DB
  ( runMQueueDBNoti
  ) where

import Sonowz.Core.DB.Pool (DBEffects, withDBConn)
import Sonowz.Core.DB.Utils (boolToException)
import Sonowz.Core.MessageQueue.Effect (MessageQueue(..))
import Sonowz.Noti.DB.Queries
import Sonowz.Noti.Imports
import Sonowz.Noti.Notification.Types (Notification)


runMQueueDBNoti :: Members DBEffects r => Sem (MessageQueue Notification : r) a -> Sem r a
runMQueueDBNoti = interpret $ \case
  Enqueue noti -> boolToException "enqueue notification"
    $ withDBConn (\conn -> liftIO $ insertNotification conn noti)
  Dequeue -> withDBConn (liftIO . selectOneNotification)
