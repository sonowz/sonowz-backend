module Sonowz.Raytrace.MessageQueue.Effect.Websocket
  ( runMQueueWebsocket
  ) where

import Sonowz.Core.MessageQueue.Effect (MessageQueue(..))
import Sonowz.Raytrace.Imports
import Sonowz.Raytrace.Websocket.Effect (WSMessage, Websocket, getWSMessage, putWSMessage)

-- "dequeue" is blocking action
runMQueueWebsocket :: Member Websocket r => Sem (MessageQueue WSMessage : r) a -> Sem r a
runMQueueWebsocket = interpret $ \case
  Enqueue msg -> putWSMessage msg
  Dequeue     -> Just <$> getWSMessage
