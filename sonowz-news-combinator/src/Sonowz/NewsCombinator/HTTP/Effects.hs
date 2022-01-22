{-# LANGUAGE TemplateHaskell #-}
module Sonowz.NewsCombinator.HTTP.Effects
  ( HTTP
  , fetchURL
  , runHTTPIO
  ) where

import qualified Control.Exception.Safe as E
import Network.HTTP.Client (HttpException, httpLbs, parseRequest, responseBody)
import Network.HTTP.Client.TLS (newTlsManager)
import Sonowz.NewsCombinator.Imports
import URI.ByteString (URI, serializeURIRef')

data HTTP m a where
  -- Returns HTML body
  FetchURL ::URI -> HTTP m Text

makeSem ''HTTP

runHTTPIO :: Members '[Embed IO , Error HttpException] r => Sem (HTTP : r) a -> Sem r a
runHTTPIO = interpret $ \case
  FetchURL url -> embed ioAction >>= fromEither   where
    ioAction :: IO (Either HttpException Text)
    ioAction = E.try $ do
      manager <- newTlsManager
      request <- parseRequest (decodeUtf8 $ serializeURIRef' url)
      decodeUtf8 . responseBody <$> httpLbs request manager
