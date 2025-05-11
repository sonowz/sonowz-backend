{-# LANGUAGE TemplateHaskell #-}

module Sonowz.Raytrace.Websocket.Effect
  ( Websocket (..),
    getWSMessage,
    putWSMessage,
    sendCloseSignal,
    receiveAny,
    WSMessage (..),
    runWebsocketToIO,
  )
where

import Network.WebSockets qualified as WS
import Polysemy.Resource (onException, resourceToIOFinal)
import Sonowz.Raytrace.Imports

newtype WSMessage = WSMessage Text deriving (Show) via Text

data Websocket m a where
  GetWSMessage :: Websocket m WSMessage
  PutWSMessage :: WSMessage -> Websocket m ()
  SendCloseSignal :: Websocket m ()
  ReceiveAny :: Websocket m ()

makeSem ''Websocket

runWebsocketToIO :: (Member (Final IO) r, Members StdEff r, HasCallStack) => WS.Connection -> Sem (Websocket : r) a -> Sem r a
runWebsocketToIO conn = interpret $ \case
  GetWSMessage -> embedFinal $ WSMessage <$> WS.receiveData conn
  PutWSMessage (WSMessage message) -> do
    logDebug $ "Websocket send data - " <> message
    embedFinal $ WS.sendTextData conn message
  SendCloseSignal -> resourceToIOFinal $ do
    logInfo "Websocket close request to client"
    onException (embedFinal $ WS.sendClose conn ("Closing connection" :: Text)) pass
  ReceiveAny -> embedFinal $ void $ WS.receive conn
