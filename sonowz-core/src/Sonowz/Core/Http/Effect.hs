{-# LANGUAGE TemplateHaskell #-}

module Sonowz.Core.Http.Effect
  ( Http,
    HttpException,
    fetchURL,
    fetchWithRequest,
    runHttpIO,
    urlToRequest,
  )
where

import Control.Exception.Safe (MonadThrow)
import Network.HTTP.Client (HttpException, Request (..), httpLbs, parseRequest, responseBody)
import Network.HTTP.Client.TLS (newTlsManager)
import Sonowz.Core.Imports
import URI.ByteString (URI, serializeURIRef')

data Http m a where
  -- Returns HTML body
  FetchURL :: URI -> Http m Text
  FetchWithRequest :: Request -> Http m Text

makeSem ''Http

-- This might raise IO exceptions, though chances are low
runHttpIO :: Members '[Embed IO, Error HttpException] r => Sem (Http : r) a -> Sem r a
runHttpIO = interpret $ \case
  FetchURL url -> fromException $ do
    manager <- newTlsManager
    request <- urlToRequest url
    decodeUtf8 . responseBody <$> httpLbs request manager
  FetchWithRequest request -> fromException $ do
    manager <- newTlsManager
    decodeUtf8 . responseBody <$> httpLbs request manager

urlToRequest :: MonadThrow m => URI -> m Request
urlToRequest url = parseRequest (decodeUtf8 $ serializeURIRef' url)