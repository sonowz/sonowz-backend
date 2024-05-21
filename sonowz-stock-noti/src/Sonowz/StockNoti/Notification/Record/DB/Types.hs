{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoStrictData #-}

module Sonowz.StockNoti.Notification.Record.DB.Types where

import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Time (Day)
import Opaleye
import Sonowz.Core.DB.Field (EmptyField, Uid)
import Sonowz.StockNoti.Imports

data StockNotiRecord' c1 c2 c3 c4 = StockNotiRecord'
  { uid :: c1,
    stockSymbol :: c2,
    notiType :: c3,
    timestamp :: c4
  }

type StockNotiRecordWriteDto = StockNotiRecord' EmptyField String String Day

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

type StockNotiRecordTable = Table StockNotiRecordW StockNotiRecordR

{- deriving via Text instance DefaultFromField SqlText StockSymbol

instance Default ToFields StockSymbol (Field SqlText) where
  def = coerce (def :: ToFields Text (Field SqlText))

instance DefaultFromField SqlText StockNotificationType where
  defaultFromField = fromMaybe (error "Invalid StockNotificationType value") . readMaybe <$> fromPGSFromField

instance Default ToFields StockNotificationType (Field SqlText) where
  def = toToFields (toFields . show @Text) -}

-- Opaleye-related stuffs --
$(makeAdaptorAndInstance "pStockNotiRecord" ''StockNotiRecord')
