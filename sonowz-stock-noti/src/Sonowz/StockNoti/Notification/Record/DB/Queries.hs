{-# LANGUAGE Arrows #-}

module Sonowz.StockNoti.Notification.Record.DB.Queries
  ( stockNotiRecordCRUD,
    findStockNotiRecord,
  )
where

import Control.Arrow (returnA)
import Control.Exception.Safe qualified as E
import Data.Profunctor (rmap)
import Data.Time (Day)
import Database.PostgreSQL.Simple (Connection)
import Opaleye
import Relude.Unsafe qualified as Unsafe
import Sonowz.Core.DB.CRUD (CRUDQueries (..), getCRUDQueries)
import Sonowz.Core.DB.Field (Uid)
import Sonowz.Core.DB.Utils (DatabaseException (DatabaseException))
import Sonowz.StockNoti.Imports
import Sonowz.StockNoti.Notification.Record.DB.Types
import Sonowz.StockNoti.Notification.Record.DB.Types qualified as Dto (StockNotiRecord' (..))
import Sonowz.StockNoti.Notification.Types (StockNotiRecord (..), StockNotificationType)
import Sonowz.StockNoti.Stock.Types (StockSymbol)

-- Table declarations --

{-
CREATE TABLE public.stock_noti_record (
    uid serial PRIMARY KEY NOT NULL,
    stock_symbol text NOT NULL,
    noti_type text NOT NULL,
    timestamp date NOT NULL
);
-}

stockNotiRecordTable :: StockNotiRecordTable
stockNotiRecordTable = table "stock_noti_record" (pStockNotiRecord fields)
  where
    fields =
      StockNotiRecord'
        { uid = tableField "uid",
          stockSymbol = tableField "stock_symbol",
          notiType = tableField "noti_type",
          timestamp = tableField "timestamp"
        }

-- Public Interfaces --

stockNotiRecordCRUD :: CRUDQueries Uid StockNotiRecordWriteDto StockNotiRecord
stockNotiRecordCRUD = rmap fromDto $ getCRUDQueries stockNotiRecordTable uid

findStockNotiRecord :: Connection -> StockSymbol -> StockNotificationType -> Day -> IO (Maybe StockNotiRecord)
findStockNotiRecord conn stockSymbol notiType timestamp = do
  runSelect conn (qSelectRecordByFields stockNotiRecordTable stockSymbol notiType timestamp) >>= \case
    [] -> return Nothing
    [record] -> return . Just . fromDto $ record
    _ -> E.throw $ DatabaseException "findStockNotiRecord: multiple instance for key"

-- Queries --

qSelectRecordByFields :: StockNotiRecordTable -> StockSymbol -> StockNotificationType -> Day -> Select StockNotiRecordR
qSelectRecordByFields table _stockSymbol _notiType _timestamp = proc () -> do
  record <- selectTable table -< ()
  restrict -< toFields (show @Text _stockSymbol) .== Dto.stockSymbol record
  restrict -< toFields (show @Text _notiType) .== Dto.notiType record
  restrict -< toFields _timestamp .== Dto.timestamp record
  returnA -< record

-- Private functions --

fromDto :: StockNotiRecordDto -> StockNotiRecord
fromDto (StockNotiRecord' _ stockSymbol notiType timestamp) =
  StockNotiRecord
    (Unsafe.read stockSymbol)
    (Unsafe.read notiType)
    timestamp