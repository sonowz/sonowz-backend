module Sonowz.Mp3tagAutofix.AudioTag.Types
  ( AudioTag(..)
  ) where

import Sonowz.Mp3tagAutofix.Imports
import Sound.HTagLib


data AudioTag = AudioTag
  { filename    :: FilePath
  , title       :: Title
  , artist      :: Artist
  , album       :: Album
  , comment     :: Comment
  , genre       :: Genre
  , year        :: Maybe Year
  , trackNumber :: Maybe TrackNumber
  }
  deriving Show
