{-# LANGUAGE TemplateHaskell #-}

module Sonowz.Mp3tagAutofix.AudioTagIO.Effect
  ( AudioTagIO,
    readAudioTag,
    writeAudioTag,
    runAudioTagIOIO,
    HTagLibException,
  )
where

import Data.Map.Strict qualified as M
import Sonowz.Mp3tagAutofix.AudioTag.Types (AudioTag (..))
import Sonowz.Mp3tagAutofix.AudioTagIO.Internal (audioTagGetter, audioTagSetter)
import Sonowz.Mp3tagAutofix.Imports
import Sound.HTagLib (HTagLibException, ID3v2Encoding (ID3v2UTF8), getTags, setTags)

data AudioTagIO m a where
  ReadAudioTag :: FilePath -> AudioTagIO m AudioTag
  WriteAudioTag :: AudioTag -> AudioTagIO m ()

makeSem ''AudioTagIO

-- Cache original tags, which are synced with filesystem values
type InternalState = State (Map FilePath AudioTag)

runAudioTagIOIO ::
  Members '[Embed IO, Error HTagLibException] r => Sem (AudioTagIO : r) a -> Sem r a
runAudioTagIOIO =
  evalState (mempty :: Map FilePath AudioTag)
    . reinterpret
      ( \case
          ReadAudioTag path -> _readAudioTag path
          WriteAudioTag audioTag -> _writeAudioTag audioTag
      )

_readAudioTag ::
  forall r.
  Members '[Embed IO, Error HTagLibException, InternalState] r =>
  FilePath ->
  Sem r AudioTag
_readAudioTag path = do
  audioTag <- fromException $ getTags path (audioTagGetter path)
  modify' (M.insert path audioTag)
  return audioTag

_writeAudioTag ::
  Members '[Embed IO, Error HTagLibException, InternalState] r => AudioTag -> Sem r ()
_writeAudioTag audioTag = do
  let path = filename audioTag
  mbOriginalTag <- M.lookup path <$> get
  case mbOriginalTag of
    Nothing -> _readAudioTag path >> _writeAudioTag audioTag
    Just originalTag -> do
      fromException $ setTags path (Just ID3v2UTF8) (audioTagSetter originalTag audioTag)
      modify' (M.insert path audioTag)
