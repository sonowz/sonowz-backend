{-# LANGUAGE TemplateHaskell #-}
module Sonowz.Raytrace.MessageQueue.Effect
  ( MessageQueue(..)
  , enqueue
  , dequeue
  )
where

import Sonowz.Raytrace.Imports

data MessageQueue msg m a where
  Enqueue :: msg -> MessageQueue msg m ()
  Dequeue :: MessageQueue msg m (Maybe msg)

makeSem ''MessageQueue
