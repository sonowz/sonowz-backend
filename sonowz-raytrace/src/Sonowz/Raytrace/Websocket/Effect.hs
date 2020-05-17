{-# LANGUAGE TemplateHaskell #-}
module Sonowz.Raytrace.Websocket.Effect
  ( Websocket(..)
  , getWSMessage
  , putWSMessage
  , sendCloseSignal
  , WSMessage(..)
  , runWebsocketToIO
  )
  where

import Polysemy.Resource (onException, resourceToIO)
import qualified Network.WebSockets as WS

import Sonowz.Raytrace.Imports

newtype WSMessage = WSMessage Text deriving (Show) via Text

-- This exception implies that close signal should be sent to the client
newtype WSCloseException = WSCloseException Text deriving (Show, Exception)

data Websocket m a where
  GetWSMessage :: Websocket m WSMessage
  PutWSMessage :: WSMessage -> Websocket m ()
  SendCloseSignal :: Websocket m ()

makeSem ''Websocket


runWebsocketToIO :: Member (Embed IO) r => WS.Connection -> Sem (Websocket : r) a -> Sem r a
runWebsocketToIO conn = interpret $ \case
  GetWSMessage -> embed $ WSMessage <$> WS.receiveData conn
  PutWSMessage (WSMessage message) -> embed $ WS.sendTextData conn message
  SendCloseSignal -> resourceToIO $ do
      -- TODO: putStrLn "closeRequest"
      onException (embed $ WS.sendClose conn ("Closing connection" :: Text)) pass
