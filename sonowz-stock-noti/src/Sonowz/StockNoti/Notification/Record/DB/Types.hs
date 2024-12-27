{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoStrictData #-}

module Sonowz.StockNoti.Notification.Record.DB.Types where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Time (Day)
import Opaleye
import Sonowz.Core.DB.Entity (Entity (..))
import Sonowz.Core.DB.Field (EmptyField, Uid)
import Sonowz.StockNoti.Imports

data StockNotiRecord' c1 c2 c3 c4 = StockNotiRecord'
  { uid :: c1,
    stockSymbol :: c2,
    notiType :: c3,
    timestamp :: c4
  }

type StockNotiRecordWriteDto = StockNotiRecord' EmptyField Text Text Day

type StockNotiRecordDto = StockNotiRecord' Uid String String Day

type StockNotiRecordW =
  StockNotiRecord'
    (Maybe (Field SqlInt4))
    (Field SqlText)
    (Field SqlText)
    (Field SqlDate)

type StockNotiRecordR =
  StockNotiRecord'
    (Field SqlInt4)
    (Field SqlText)
    (Field SqlText)
    (Field SqlDate)

instance Entity StockNotiRecordR where
  entityIdField = uid
  entityToFields _ = toFields

type StockNotiRecordTable = Table StockNotiRecordW StockNotiRecordR

-- Opaleye-related stuffs --
$(makeAdaptorAndInstance "pStockNotiRecord" ''StockNotiRecord')
