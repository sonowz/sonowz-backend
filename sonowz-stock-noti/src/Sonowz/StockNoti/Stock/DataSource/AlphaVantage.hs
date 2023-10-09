{-# LANGUAGE QuasiQuotes #-}

module Sonowz.StockNoti.Stock.DataSource.AlphaVantage
  ( runStockDataSourceAlphaVantage,
  )
where

import Data.Aeson
import Data.Aeson.Key qualified as A
import Data.Aeson.KeyMap (keys)
import Data.Aeson.Types (Parser)
import Data.ByteString (isInfixOf)
import Data.Time (UTCTime (..), secondsToDiffTime)
import Data.Time.Calendar (Day)
import Relude.Extra (un)
import Sonowz.Core.Exception.Types (NotImplementedException (..), ParseException (..))
import Sonowz.Core.HTTP.Effect (HTTP, HttpException, fetchURL, runHTTPIO)
import Sonowz.StockNoti.Imports
import Sonowz.StockNoti.Stock.DataSource.Effect (StockDataSource (..))
import Sonowz.StockNoti.Stock.Types (StockPrice (..), StockSymbol (..), StockTimeSeries (..))
import URI.ByteString
import URI.ByteString.QQ (uri)

runStockDataSourceAlphaVantage :: (Member (Embed IO) r, Members StdEff r) => Sem (StockDataSource : r) a -> Sem r a
runStockDataSourceAlphaVantage =
  mapError @HttpException toException
    . runHTTPIO
    . reinterpret2
      ( \case
          FetchYearStockPrices _ -> throw' (NotImplemented "year API does not exist")
          FetchMonthStockPrices symbol -> coerce <$> fetchTimeSeries "MONTHLY" symbol
          FetchWeekStockPrices symbol -> coerce <$> fetchTimeSeries "WEEKLY" symbol
          FetchDayStockPrices symbol -> coerce <$> fetchTimeSeries "DAILY" symbol
          FetchHourStockPrices _ -> throw' (NotImplemented "hour is not implemented yet")
      )

fetchTimeSeries :: (Member HTTP r, Members StdEff r, HasCallStack) => Text -> StockSymbol -> Sem r (StockTimeSeries tu)
fetchTimeSeries apiTimeUnit symbol = do
  logInfo $ "Started fetching stock time series from AlphaVantage (" <> show symbol <> ")"
  decoded <- eitherDecode . encodeUtf8 <$> fetchURL url
  timeSeries <- mapToStockTimeSeries <$> handleEither decoded
  logInfo $ "Fetched count: " <> (show . length . prices) timeSeries <> "."
  return timeSeries
  where
    url = [uri|https://www.alphavantage.co/query|] {uriQuery = queryParams}
    queryParams =
      Query
        [ ("function", "TIME_SERIES_" <> encodeUtf8 apiTimeUnit),
          ("symbol", encodeUtf8 $ un @Text symbol),
          ("datatype", "json"),
          ("apikey", "3IRIGGU7NXGC9P6E")
        ]
    handleEither = fromEither . first (toException . ParseException . toText)

-- JSON response parsing --

data APIResponse = APIResponse
  { meta :: APIResponseMeta,
    timeSeries :: [APIResponseTimeSeries]
  }
  deriving (Show, Generic)

data APIResponseMeta = APIResponseMeta
  { information :: Text,
    stockSymbol :: StockSymbol,
    lastRefreshed :: Day,
    outputSize :: Maybe Text,
    timeZone :: Maybe Text
  }
  deriving (Show, Generic)

data APIResponseTimeSeries = APIResponseTimeSeries
  { time :: UTCTime,
    open :: Double,
    high :: Double,
    low :: Double,
    close :: Double,
    volume :: Double
  }
  deriving (Show, Generic)

deriving instance Generic StockSymbol

deriving newtype instance FromJSON StockSymbol

instance FromJSON APIResponse where
  parseJSON = withObject "APIResponse" $ \obj -> do
    meta <- obj .: "Meta Data"
    let maybeKey = find (\k -> "Time Series" `isInfixOf` show k) (keys obj)
    timeSeriesValue <- case maybeKey of
      Just key -> obj .: key
      Nothing -> fail "Time Series not found"
    timeSeries <- withObject "Time Series Object" parseTimeSeries timeSeriesValue
    pure $ APIResponse meta timeSeries
    where
      parseTimeSeries :: Object -> Parser [APIResponseTimeSeries]
      parseTimeSeries obj = traverse parseTimeSeriesItem (keys obj)
        where
          parseTimeSeriesItem timeKey = do
            time <- parseTime timeKey
            ($ time) <$> obj .: timeKey
      parseTime :: Key -> Parser UTCTime
      parseTime (A.toString -> key) = fromUTCTime <|> fromDay <|> fail key
        where
          fromUTCTime = whenNothing (readMaybe @UTCTime key) (fail "")
          fromDay = whenNothing (toUTCTime <$> readMaybe @Day key) (fail "")
      toUTCTime :: Day -> UTCTime
      toUTCTime day = UTCTime day (secondsToDiffTime 0)

instance FromJSON APIResponseMeta where
  parseJSON = withObject "APIResponseMeta" $ \obj -> do
    information <- obj .: "1. Information"
    stockSymbol <- obj .: "2. Symbol"
    lastRefreshed <- obj .: "3. Last Refreshed"
    outputSize <- obj .:? "4. Output Size"
    timeZone <- obj .:? "5. Time Zone"
    pure $ APIResponseMeta {..}

instance FromJSON (UTCTime -> APIResponseTimeSeries) where
  parseJSON = withObject "APIResponseTimeSeries" $ \obj -> do
    open <- obj .: "1. open" >>= readDouble
    high <- obj .: "2. high" >>= readDouble
    low <- obj .: "3. low" >>= readDouble
    close <- obj .: "4. close" >>= readDouble
    volume <- obj .: "5. volume" >>= readDouble
    pure $ \time -> APIResponseTimeSeries {..}
    where
      readDouble :: String -> Parser Double
      readDouble str = whenNothing (readMaybe str) (fail str)

mapToStockTimeSeries :: APIResponse -> StockTimeSeries tu
mapToStockTimeSeries (APIResponse meta timeSeries) =
  StockTimeSeries
    { symbol = stockSymbol meta,
      name = un (stockSymbol meta),
      prices = mapToTimeSeries <$> timeSeries
    }

mapToTimeSeries :: APIResponseTimeSeries -> StockPrice tu
mapToTimeSeries (APIResponseTimeSeries {..}) = StockPrice {..}
