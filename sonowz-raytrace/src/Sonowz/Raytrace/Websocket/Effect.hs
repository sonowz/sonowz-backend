{-# LANGUAGE TemplateHaskell #-}
module Sonowz.Raytrace.Websocket.Effect
  ( Websocket(..)
  , getWSMessage
  , putWSMessage
  , sendCloseSignal
  , receiveAny
  , WSMessage(..)
  , runWebsocketToIO
  )
  where

import Polysemy.Resource (onException, resourceToIO)
import qualified Network.WebSockets as WS

import Sonowz.Raytrace.Imports

newtype WSMessage = WSMessage Text deriving (Show) via Text

data Websocket m a where
  GetWSMessage :: Websocket m WSMessage
  PutWSMessage :: WSMessage -> Websocket m ()
  SendCloseSignal :: Websocket m ()
  ReceiveAny :: Websocket m ()

makeSem ''Websocket


runWebsocketToIO :: Member (Embed IO) r => WS.Connection -> Sem (Websocket : r) a -> Sem r a
runWebsocketToIO conn = interpret $ \case
  GetWSMessage -> embed $ WSMessage <$> WS.receiveData conn
  PutWSMessage (WSMessage message) -> embed $ WS.sendTextData conn message
  SendCloseSignal -> resourceToIO $ do
      -- TODO: putStrLn "closeRequest"
      onException (embed $ WS.sendClose conn ("Closing connection" :: Text)) pass
  ReceiveAny -> embed $ void $ WS.receive conn
