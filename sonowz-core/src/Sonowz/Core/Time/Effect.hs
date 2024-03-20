{-# LANGUAGE TemplateHaskell #-}

module Sonowz.Core.Time.Effect
  ( Time (..),
    threadDelay,
    timeout,
    getTime,
    timeToIO,
  )
where

import Control.Concurrent qualified as T
import Data.Time.LocalTime (ZonedTime, getZonedTime)
import Sonowz.Core.Imports
import System.Timeout qualified as T

data Time m a where
  ThreadDelay :: Int -> Time m ()
  Timeout :: Int -> m a -> Time m (Maybe a)
  GetTime :: Time m ZonedTime

makeSem ''Time

timeToIO :: Member (Embed IO) r => Sem (Time : r) a -> Sem r a
timeToIO = interpretH $ \case
  ThreadDelay microsec -> pureT =<< liftIO (T.threadDelay microsec)
  Timeout microsec action -> do
    action' <- runT action
    nothing <- pureT Nothing
    withLowerToIO $ \lower _ -> do
      let done = lower . raise . timeToIO
          -- sequence' = maybe nothing sequence
          sequence' = \case
            Nothing -> nothing
            Just x -> Just <$> x
      sequence' <$> T.timeout microsec (done action')
  GetTime -> pureT =<< liftIO getZonedTime
