{-# LANGUAGE QuasiQuotes #-}
module Sonowz.Mp3tagAutofix.AudioTag.Autofix.Types
  ( ArtistPool
  , ArtistPoolWithSearchResult
  , SearchResult(..)
  , SearchResultArtist(..)
  , SearchResultSong(..)
  , Song(..)
  , MelonSearchType(..)
  , melonSearchUrl
  ) where

import Sonowz.Mp3tagAutofix.Imports

import Sonowz.Mp3tagAutofix.AudioTag.Types
  (Artist, AudioTag, Title, joinArtistList, unArtist, unTitle)
import qualified Text.Show as S
import URI.ByteString (URI, queryL, queryPairsL)
import URI.ByteString.QQ (uri)


-- Concatenated tags where artists are unique
type ArtistPool = Map Artist (NonEmpty AudioTag)
type ArtistPoolWithSearchResult = Map Artist (AudioTag, SearchResult)


data SearchResult = SearchResult
  { artistSection :: SearchResultArtist
  , songSection   :: SearchResultSong
  }
  deriving Show
newtype SearchResultArtist = SearchResultArtist [Artist]
newtype SearchResultSong = SearchResultSong [Song] deriving (Semigroup, Monoid) via [Song]
data Song = Song (NonEmpty Artist) Title
  deriving (Eq, Ord)


instance S.Show SearchResultArtist where
  show (SearchResultArtist artists) =
    "[" <> intercalate ", " (show . unArtist <$> toList artists) <> "]"
instance S.Show SearchResultSong where
  show (SearchResultSong songs) = "[" <> intercalate ", " (show <$> toList songs) <> "]"
instance S.Show Song where
  show (Song artists title) =
    "<" <> show (unArtist $ joinArtistList artists) <> "> " <> show (unTitle title)


data MelonSearchType = MelonAll | MelonArtist | MelonTitle

-- https://news.google.com/rss/search?cr=false&hl=en-US&gl=US&ceid=US:en&q=key+word
melonSearchUrl :: MelonSearchType -> Text -> URI
melonSearchUrl searchType keyword = addQueryParam
  ("q", encodeUtf8 $ encodeQueryParam keyword)
  baseURL
 where
  baseURL = case searchType of
    MelonAll    -> [uri|https://www.melon.com/search/song/index.htm?section=all|]
    MelonArtist -> [uri|https://www.melon.com/search/artist/index.htm|]
    MelonTitle  -> [uri|https://www.melon.com/search/song/index.htm?section=song|]
  encodeQueryParam :: Text -> Text
  encodeQueryParam (toString -> param) = toText $ map (\c -> if c == ' ' then '+' else c) param
  addQueryParam :: (ByteString, ByteString) -> URI -> URI
  addQueryParam param = runIdentity <$> (queryL . queryPairsL) (Identity . (:) param)
