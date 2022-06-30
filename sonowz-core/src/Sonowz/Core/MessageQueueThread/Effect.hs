{-# LANGUAGE TemplateHaskell #-}

module Sonowz.Core.MessageQueueThread.Effect
  ( MessageQueueStream (..),
    doStreamLoop,
    StreamHandler,
    StreamResult (..),
    runMQueueStream,
  )
where

import Sonowz.Core.Imports
import Sonowz.Core.MessageQueue.Effect (MessageQueue, dequeue, enqueue)
import Sonowz.Core.Time.Effect (Time, threadDelay)

data MessageQueueStream rx tx m a where
  DoStreamLoop :: MessageQueueStream rx tx m ()

makeSem ''MessageQueueStream

type StreamHandler r rx tx = rx -> Sem r (StreamResult tx)

data StreamResult tx = HSend tx | HSendTerminate tx | HContinue | HTerminate

runMQueueStream ::
  Members '[Time, MessageQueue rx, MessageQueue tx] r =>
  StreamHandler r rx tx ->
  Sem (MessageQueueStream rx tx : r) a ->
  Sem r a
runMQueueStream handler = interpret $ \case
  DoStreamLoop -> runStreamLoop handler

runStreamLoop ::
  Members '[Time, MessageQueue rx, MessageQueue tx] r => StreamHandler r rx tx -> Sem r ()
runStreamLoop handler = do
  message :: rx <- dequeueBlocking
  result :: StreamResult tx <- handler message
  case result of
    HContinue -> runStreamLoop handler
    HTerminate -> pass
    HSend message -> enqueue message >> runStreamLoop handler
    HSendTerminate message -> enqueue message >> pass

dequeueBlocking :: Members '[Time, MessageQueue rx] r => Sem r rx
dequeueBlocking =
  dequeue >>= \case
    Just message -> return message
    Nothing -> threadDelay (10 ^ 6) >> dequeueBlocking
