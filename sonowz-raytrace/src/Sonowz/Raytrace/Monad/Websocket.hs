{-# LANGUAGE UndecidableInstances #-}
module Sonowz.Raytrace.Monad.Websocket
  ( MonadWebsocket (..)
  , WSMessage (..)
  ) where

import Relude
import qualified Network.WebSockets as WS
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (throwIO, onException)
import UnliftIO.Timeout (timeout)

import Sonowz.Raytrace.Core.Has (MonadHas(..))


class Monad m => MonadWebsocket m where
  getWSMessage :: m WSMessage
  putWSMessage :: WSMessage -> m ()
  wrapTimeout :: forall a . Int -> m a -> m a -- Handles timeout & throws exception
  sendCloseSignal :: m ()

newtype WSMessage = WSMessage Text deriving (Show) via Text

-- This exception implies that close signal should be sent to the client
newtype WSCloseException = WSCloseException Text deriving (Show, Exception)

-- TODO: logging
-- TODO: make this as monad transformer and automatically wrap with sendCloseSignal
-- This monad should be wrapped with 'finally' to 'sendCloseSignal' to the client
instance (MonadHas WS.Connection m, Monad m, MonadUnliftIO m) => MonadWebsocket m where
  getWSMessage = do
    conn <- grab @WS.Connection
    WSMessage <$> liftIO (WS.receiveData conn)
  putWSMessage (WSMessage message) = do
    conn <- grab @WS.Connection
    liftIO $ WS.sendTextData conn message
  wrapTimeout sec action = timeout (sec * 10 ^ 6) action >>= \case
    Just result -> return result
    Nothing     -> throwIO (WSCloseException "connection timed out")
  sendCloseSignal = do
    conn <- grab @WS.Connection
    putStrLn "closeRequest"
    onException (liftIO $ WS.sendClose conn ("Closing connection" :: Text)) pass
