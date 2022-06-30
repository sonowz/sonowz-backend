{-# LANGUAGE TemplateHaskell #-}

module Sonowz.Core.MessageQueue.Effect
  ( MessageQueue (..),
    enqueue,
    dequeue,
  )
where

import Sonowz.Core.Imports

data MessageQueue msg m a where
  Enqueue :: msg -> MessageQueue msg m ()
  Dequeue :: MessageQueue msg m (Maybe msg)

makeSem ''MessageQueue
