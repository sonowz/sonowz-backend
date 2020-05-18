{-# LANGUAGE TemplateHaskell #-}
module Sonowz.Raytrace.MessageQueueThread.Effect
  ( MessageQueueStream(..)
  , doStreamLoop
  , StreamHandler
  , StreamResult(..)
  , runMQueueStream
  )
  where
  
import Sonowz.Raytrace.Imports
import Sonowz.Raytrace.MessageQueue.Effect (MessageQueue, enqueue, dequeue)
import Sonowz.Raytrace.Time.Effect (Time, threadDelay)

data MessageQueueStream rx tx m a where
  DoStreamLoop :: MessageQueueStream rx tx m ()

makeSem ''MessageQueueStream


type StreamHandler r rx tx = rx -> Sem r (StreamResult tx)
data StreamResult tx = HSend tx | HSendTerminate tx | HContinue | HTerminate

type InOutQueues r rx tx = (MessageQueue rx : MessageQueue tx : r)

runMQueueStream
  :: Member Time r
  => StreamHandler (InOutQueues r rx tx) rx tx
  -> Sem (MessageQueueStream rx tx : r) a
  -> Sem (InOutQueues r rx tx) a
runMQueueStream handler = reinterpret2 $ \case
  DoStreamLoop -> runStreamLoop handler
    

runStreamLoop :: Member Time r
  => StreamHandler (InOutQueues r rx tx) rx tx -> Sem (InOutQueues r rx tx) ()
runStreamLoop handler = do
  message :: rx <- dequeueBlocking
  result :: StreamResult tx <- handler message
  case result of
    HContinue              -> runStreamLoop handler
    HTerminate             -> pass
    HSend          message -> raise (enqueue message) >> runStreamLoop handler
    HSendTerminate message -> raise (enqueue message) >> pass

dequeueBlocking :: Members [Time, MessageQueue rx] r => Sem r rx
dequeueBlocking = dequeue >>= \case
  Just message -> return message
  Nothing      -> threadDelay (10^6) >> dequeueBlocking
