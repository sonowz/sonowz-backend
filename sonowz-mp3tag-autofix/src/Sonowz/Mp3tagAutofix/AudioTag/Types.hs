module Sonowz.Mp3tagAutofix.AudioTag.Types
  ( AudioTag(..)
  , Encoding(..)
  , artistList
  , joinArtistList
  -- Reexport "Sound.HTagLib"
  , Title
  , Artist
  , Album
  , Comment
  , Genre
  , Year
  , TrackNumber
  , mkTitle
  , mkArtist
  , mkAlbum
  , mkComment
  , mkGenre
  , mkYear
  , mkTrackNumber
  , unTitle
  , unArtist
  , unAlbum
  , unComment
  , unGenre
  , unYear
  , unTrackNumber
  ) where

import Sonowz.Mp3tagAutofix.Imports
import Sound.HTagLib

import qualified Data.Text as T


data AudioTag = AudioTag
  { encoding    :: Encoding
  , filename    :: FilePath
  , title       :: Title
  , artist      :: Artist
  , album       :: Album
  , comment     :: Comment
  , genre       :: Genre
  , year        :: Maybe Year
  , trackNumber :: Maybe TrackNumber
  }
  deriving (Show, Eq)

data Encoding = EncodingUtf8 | EncodingOther deriving (Show, Eq)



-- Handles "A, B" or "A & B" cases
artistList :: Artist -> NonEmpty Artist
artistList = fromList . fmap (mkArtist . T.strip) . T.split (`elem` (",&" :: String)) . unArtist

joinArtistList :: NonEmpty Artist -> Artist
joinArtistList = mkArtist . T.intercalate ", " . fmap unArtist . toList
