{-# LANGUAGE TemplateHaskell #-}
module Sonowz.Raytrace.Time.Effect
  ( Time(..)
  , threadDelay
  , timeout
  , getTime
  , timeToIO
  )
  where

import Data.Time.LocalTime (ZonedTime, getZonedTime)
import qualified Control.Concurrent as T
import qualified System.Timeout as T

import Sonowz.Raytrace.Imports

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
      let done = (lower . raise . timeToIO)
      T.timeout microsec (done action') >>= \case
        Just x -> return (Just <$> x)
        Nothing -> return nothing
  GetTime -> pureT =<< liftIO getZonedTime 
