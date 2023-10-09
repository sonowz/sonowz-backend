module Sonowz.StockNoti.Notification.Types
  ( StockNotificationType (..),
  )
where

import Sonowz.StockNoti.Imports

data StockNotificationType = NotiGoldenCross | NotiDeadCross deriving (Eq, Show, Read)