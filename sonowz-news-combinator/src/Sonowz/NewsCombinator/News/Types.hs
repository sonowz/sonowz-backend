{-# LANGUAGE QuasiQuotes #-}

module Sonowz.NewsCombinator.News.Types
  ( NewsItem (..),
    googleNewsRSSUrl,
  )
where

import Data.Time (UTCTime)
import Sonowz.NewsCombinator.Imports
import Text.Show qualified as S
import URI.ByteString (URI, queryL, queryPairsL)
import URI.ByteString.QQ (uri)

data NewsItem = NewsItem
  { getTitle :: Text,
    getDate :: UTCTime,
    getAttrs :: Map Text Text
  }

-- TODO: relude >= 1.0 might changed how 'Show' works...
instance Show NewsItem where
  show NewsItem {..} = toString (show getDate <> " : [" <> getTitle <> "] " <> show getAttrs)

-- https://news.google.com/rss/search?cr=false&hl=en-US&gl=US&ceid=US:en&q=key+word
googleNewsRSSUrl :: Text -> URI
googleNewsRSSUrl keyword = addQueryParam ("q", encodeUtf8 $ encodeQueryParam keyword) baseURL
  where
    baseURL = [uri|https://news.google.com/rss/search?cr=false&hl=en-US&gl=US&ceid=US:en|]
    encodeQueryParam :: Text -> Text
    encodeQueryParam (toString -> param) = toText $ map (\c -> if c == ' ' then '+' else c) param
    addQueryParam :: (ByteString, ByteString) -> URI -> URI
    addQueryParam param = runIdentity <$> (queryL . queryPairsL) (Identity . (:) param)
