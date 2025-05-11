{-# LANGUAGE TemplateHaskell #-}

module Sonowz.Core.Time.Effect
  ( Time (..),
    threadDelay,
    timeout,
    getTime,
    timeToIOFinal,
  )
where

import Control.Concurrent qualified as T
import Data.Time.LocalTime (ZonedTime, getZonedTime)
import Polysemy.Final (getInspectorS, interpretFinal)
import Polysemy.Internal.Strategy (liftS, runS)
import Sonowz.Core.Imports
import System.Timeout qualified as T

data Time m a where
  ThreadDelay :: Integer -> Time m ()
  Timeout :: Int -> m a -> Time m (Maybe a)
  GetTime :: Time m ZonedTime

makeSem ''Time

timeToIOFinal :: (Member (Final IO) r) => Sem (Time : r) a -> Sem r a
timeToIOFinal = interpretFinal $ \case
  ThreadDelay microsec -> liftS (threadDelayInteger microsec)
  Timeout microsec action -> do
    ins <- getInspectorS
    action' <- runS action
    let result :: IO (Maybe (Maybe _)) = T.timeout microsec (inspect ins <$> action')
    liftS $ join <$> result
  GetTime -> liftS getZonedTime

threadDelayInteger :: Integer -> IO ()
threadDelayInteger t
  | t > 10 ^ 9 = T.threadDelay (10 ^ 9) >> threadDelayInteger (t - 10 ^ 9)
  | otherwise = T.threadDelay (fromIntegral t)