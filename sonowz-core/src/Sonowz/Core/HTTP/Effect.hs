{-# LANGUAGE TemplateHaskell #-}

module Sonowz.Core.HTTP.Effect
  ( HTTP,
    HttpException,
    fetchURL,
    fetchWithRequest,
    runHTTPIO,
    urlToRequest,
  )
where

import Control.Exception.Safe (MonadThrow)
import Network.HTTP.Client (HttpException, Request (..), httpLbs, parseRequest, responseBody)
import Network.HTTP.Client.TLS (newTlsManager)
import Sonowz.Core.Imports
import URI.ByteString (URI, serializeURIRef')

data HTTP m a where
  -- Returns HTML body
  FetchURL :: URI -> HTTP m Text
  FetchWithRequest :: Request -> HTTP m Text

makeSem ''HTTP

-- This might raise IO exceptions, though chances are low
runHTTPIO :: Members '[Embed IO, Error HttpException] r => Sem (HTTP : r) a -> Sem r a
runHTTPIO = interpret $ \case
  FetchURL url -> fromException $ do
    manager <- newTlsManager
    request <- urlToRequest url
    decodeUtf8 . responseBody <$> httpLbs request manager
  FetchWithRequest request -> fromException $ do
    manager <- newTlsManager
    decodeUtf8 . responseBody <$> httpLbs request manager

urlToRequest :: MonadThrow m => URI -> m Request
urlToRequest url = parseRequest (decodeUtf8 $ serializeURIRef' url)