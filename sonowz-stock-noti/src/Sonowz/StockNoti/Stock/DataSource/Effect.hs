{-# LANGUAGE TemplateHaskell #-}

module Sonowz.StockNoti.Stock.DataSource.Effect
  ( StockDataSource (..),
    fetchYearStockPrices,
    fetchMonthStockPrices,
    fetchWeekStockPrices,
    fetchDayStockPrices,
    fetchHourStockPrices,
  )
where

import Sonowz.StockNoti.Imports
import Sonowz.StockNoti.Stock.Types (StockSymbol, StockTimeSeries, TimeUnit (..))

data StockDataSource m a where
  FetchYearStockPrices :: StockSymbol -> StockDataSource m (StockTimeSeries TYear)
  FetchMonthStockPrices :: StockSymbol -> StockDataSource m (StockTimeSeries TMonth)
  FetchWeekStockPrices :: StockSymbol -> StockDataSource m (StockTimeSeries TWeek)
  FetchDayStockPrices :: StockSymbol -> StockDataSource m (StockTimeSeries TDay)
  FetchHourStockPrices :: StockSymbol -> StockDataSource m (StockTimeSeries THour)

makeSem ''StockDataSource
