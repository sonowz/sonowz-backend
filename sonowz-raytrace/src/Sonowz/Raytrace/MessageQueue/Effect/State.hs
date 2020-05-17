module Sonowz.Raytrace.MessageQueue.Effect.State
  ( runMQueueState
  , removeMQueueState
  )
where

import Data.List (partition)
import Polysemy.AtomicState (AtomicState(..), atomicState')

import Sonowz.Raytrace.Imports
import Sonowz.Raytrace.MessageQueue.Effect (MessageQueue(..))

runMQueueState :: Member (AtomicState [msg]) r => Sem (MessageQueue msg : r) a -> Sem r a
runMQueueState = interpret $ \case
  Enqueue msg -> atomicState' $ \l -> (l <> [msg], ())
  Dequeue -> atomicState' f where
    f (msg : msgs) = (msgs, Just msg)
    f l        = (l, Nothing)

-- Returns False if no item were removed
removeMQueueState :: Member (AtomicState [msg]) r => (msg -> Bool) -> Sem r Bool
removeMQueueState p = atomicState' f
  where f l = let (rs, xs) = partition p l in (xs, not $ null rs)
