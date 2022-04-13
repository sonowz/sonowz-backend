{-# LANGUAGE TemplateHaskell #-}
module Sonowz.Mp3tagAutofix.AudioTagIO.Effect
  ( AudioTagIO
  , readAudioTag
  , writeAudioTag
  , runAudioTagIOIO
  , HTagLibException
  ) where

import Sonowz.Mp3tagAutofix.Imports
import Sound.HTagLib

import qualified Data.Map.Strict as M
import Sonowz.Mp3tagAutofix.AudioTag.Types (AudioTag(..))


data AudioTagIO m a where
  ReadAudioTag ::FilePath -> AudioTagIO m AudioTag
  WriteAudioTag ::AudioTag -> AudioTagIO m ()

makeSem ''AudioTagIO


-- Cache original tags, which are synced with filesystem values
type InternalState = State (Map FilePath AudioTag)

runAudioTagIOIO
  :: Members '[Embed IO , Error HTagLibException] r => Sem (AudioTagIO : r) a -> Sem r a
runAudioTagIOIO = evalState (mempty :: Map FilePath AudioTag) . reinterpret
  (\case
    ReadAudioTag  path     -> _readAudioTag path
    WriteAudioTag audioTag -> _writeAudioTag audioTag
  )

_readAudioTag
  :: forall r
   . Members '[Embed IO , Error HTagLibException , InternalState] r
  => FilePath
  -> Sem r AudioTag
_readAudioTag path = do
  audioTag <- fromException $ getTags path (audioTagGetter path)
  modify' (M.insert path audioTag)
  return audioTag

_writeAudioTag
  :: Members '[Embed IO , Error HTagLibException , InternalState] r => AudioTag -> Sem r ()
_writeAudioTag audioTag = do
  let path = filename audioTag
  mbOriginalTag <- M.lookup path <$> get
  case mbOriginalTag of
    Nothing          -> _readAudioTag path >> _writeAudioTag audioTag
    Just originalTag -> do
      fromException $ setTags path (Just ID3v2UTF8) (audioTagSetter originalTag audioTag)
      modify' (M.insert path audioTag)


audioTagGetter :: FilePath -> TagGetter AudioTag
audioTagGetter filename =
  AudioTag filename
    <$> titleGetter
    <*> artistGetter
    <*> albumGetter
    <*> commentGetter
    <*> genreGetter
    <*> yearGetter
    <*> trackNumberGetter

-- Only set fields which has been changed from original tag
audioTagSetter :: AudioTag -> AudioTag -> TagSetter
audioTagSetter orig target =
  setterIfChanged title titleSetter
    <> setterIfChanged artist      artistSetter
    <> setterIfChanged album       albumSetter
    <> setterIfChanged comment     commentSetter
    <> setterIfChanged genre       genreSetter
    <> setterIfChanged year        yearSetter
    <> setterIfChanged trackNumber trackNumberSetter
 where
  setterIfChanged :: Eq a => (AudioTag -> a) -> (a -> TagSetter) -> TagSetter
  setterIfChanged field setter =
    if field orig /= field target then setter (field target) else mempty
