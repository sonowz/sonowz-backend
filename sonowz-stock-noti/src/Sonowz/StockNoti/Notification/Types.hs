module Sonowz.StockNoti.Notification.Types
  ( StockNotificationType (..),
    StockNotiRecord (..),
  )
where

import Data.Time (Day)
import Sonowz.StockNoti.Imports
import Sonowz.StockNoti.Stock.Types (StockSymbol)

data StockNotificationType = NotiGoldenCross | NotiDeadCross deriving (Eq, Show, Read)

data StockNotiRecord = StockNotiRecord
  { stockSymbol :: StockSymbol,
    notiType :: StockNotificationType,
    timestamp :: Day
  }
  deriving (Show, Generic)