{-# LANGUAGE TemplateHaskell #-}
module Sonowz.Core.HTTP.Effect
  ( HTTP
  , fetchURL
  , runHTTPIO
  ) where

import Network.HTTP.Client (HttpException, httpLbs, parseRequest, responseBody)
import Network.HTTP.Client.TLS (newTlsManager)
import Sonowz.Core.Imports
import URI.ByteString (URI, serializeURIRef')

data HTTP m a where
  -- Returns HTML body
  FetchURL ::URI -> HTTP m Text

makeSem ''HTTP

-- This might raise IO exceptions, though chances are low
runHTTPIO :: Members '[Embed IO , Error HttpException] r => Sem (HTTP : r) a -> Sem r a
runHTTPIO = interpret $ \case
  FetchURL url -> fromException $ do
    manager <- newTlsManager
    request <- parseRequest (decodeUtf8 $ serializeURIRef' url)
    decodeUtf8 . responseBody <$> httpLbs request manager
