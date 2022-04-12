{-# LANGUAGE QuasiQuotes #-}
module Sonowz.Mp3tagAutofix.AudioTag.Autofix.Types
  ( ArtistPool
  , ArtistPoolWithSearchResult
  , SearchResult(..)
  , SearchResultArtist(..)
  , SearchResultSong(..)
  , Song(..)
  , melonSearchUrl
  ) where

import Sonowz.Mp3tagAutofix.Imports

import Sonowz.Mp3tagAutofix.AudioTag.Types
  (Artist, AudioTag, Title, joinArtistList, unArtist, unTitle)
import qualified Text.Show as S
import URI.ByteString (URI, queryL, queryPairsL)
import URI.ByteString.QQ (uri)


-- Concatenated tags
-- where artists are unique with representative tag
type ArtistPool = Map Artist AudioTag
type ArtistPoolWithSearchResult = Map Artist (AudioTag, SearchResult)


data SearchResult = SearchResult
  { artistSection :: SearchResultArtist
  , songSection   :: SearchResultSong
  }
  deriving Show
newtype SearchResultArtist = SearchResultArtist [Artist]
newtype SearchResultSong = SearchResultSong [Song]
data Song = Song (NonEmpty Artist) Title
  deriving (Eq, Ord)

-- Override Show instances, since "show Text" escapes non-ASCII characters
instance S.Show SearchResultArtist where
  show (SearchResultArtist artists) =
    "[" <> intercalate ", " (toString . unArtist <$> toList artists) <> "]"
instance S.Show SearchResultSong where
  show (SearchResultSong songs) = "[" <> intercalate ", " (show <$> toList songs) <> "]"
instance S.Show Song where
  show (Song artists title) =
    "<" <> toString (unArtist $ joinArtistList artists) <> "> " <> toString (unTitle title)


-- https://news.google.com/rss/search?cr=false&hl=en-US&gl=US&ceid=US:en&q=key+word
melonSearchUrl :: Text -> URI
melonSearchUrl keyword = addQueryParam ("q", encodeUtf8 $ encodeQueryParam keyword) baseURL where
  baseURL = [uri|https://www.melon.com/search/total/index.htm|]
  encodeQueryParam :: Text -> Text
  encodeQueryParam (toString -> param) = toText $ map (\c -> if c == ' ' then '+' else c) param
  addQueryParam :: (ByteString, ByteString) -> URI -> URI
  addQueryParam param = runIdentity <$> (queryL . queryPairsL) (Identity . (:) param)
