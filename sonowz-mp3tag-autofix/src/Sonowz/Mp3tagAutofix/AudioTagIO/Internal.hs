module Sonowz.Mp3tagAutofix.AudioTagIO.Internal
  ( audioTagGetter
  , audioTagSetter
  ) where

import Sonowz.Mp3tagAutofix.Imports
import Sound.HTagLib

import qualified Data.Text as T
import Data.Text.ICU.CharsetDetection (detect, getConfidence, getName)
import Data.Text.ICU.Convert (fromUnicode, open, toUnicode)
import Relude.Extra.Tuple (dup)
import Sonowz.Mp3tagAutofix.AudioTag.Types (AudioTag(..))
import System.IO.Unsafe (unsafePerformIO)


audioTagGetter :: FilePath -> TagGetter AudioTag
audioTagGetter filename =
  AudioTag filename
    <$> (mkTitle . autofixEncoding . unTitle <$> titleGetter)
    <*> (mkArtist . autofixEncoding . unArtist <$> artistGetter)
    <*> (mkAlbum . autofixEncoding . unAlbum <$> albumGetter)
    <*> (mkComment . autofixEncoding . unComment <$> commentGetter)
    <*> (mkGenre . autofixEncoding . unGenre <$> genreGetter)
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


-- Fix encodings where 'htaglib' fails to detect, especially 'CP949'
autofixEncoding :: HasCallStack => Text -> Text
autofixEncoding text
  | isProperText text                 = text
  | isProperText (latin1ToCP949 text) = latin1ToCP949 text
  | isProperText (utf8ToCP949 text)   = utf8ToCP949 text
  | isProperText (utf8ToLatin1 text)  = utf8ToLatin1 text
  | otherwise                         = autoDetect text where
  -- Tag words must start with [A-Za-z0-9가-힣]
  isProperText :: Text -> Bool
  isProperText = all go . words . T.filter (/= '(') . T.strip   where
    go :: Text -> Bool
    go t = t == "" || ("0" <= t && t <= "z") || ("가" <= t && t <= "힣")
  converterCP949  = unsafePerformIO $ open "cp949" Nothing
  converterLatin1 = unsafePerformIO $ open "ISO8859-1" Nothing
  utf8ToCP949 :: Text -> Text
  utf8ToCP949 = toUnicode converterCP949 . encodeUtf8
  utf8ToLatin1 :: Text -> Text
  utf8ToLatin1 = toUnicode converterLatin1 . encodeUtf8
  latin1ToCP949 :: Text -> Text
  latin1ToCP949 = toUnicode converterCP949 . fromUnicode converterLatin1
  -- Use autodetect function in 'Data.Text.ICU.CharsetDetection'
  autoDetect :: Text -> Text
  autoDetect text = unsafePerformIO $ do
    let
      encoded1 = fromUnicode converterLatin1 text
      encoded2 = encodeUtf8 text
      bitraverse' :: Applicative m => (a -> m b) -> (a -> m c) -> a -> m (b, c)
      bitraverse' f g = bitraverse f g . dup
    (score1, decoding1) <- bitraverse' getConfidence getName =<< detect encoded1
    (score2, decoding2) <- bitraverse' getConfidence getName =<< detect encoded2
    let
      (encoded, decoding) =
        if score1 >= score2 then (encoded1, decoding1) else (encoded2, decoding2)
    converter <- open (toString decoding) Nothing
    return $ toUnicode converter encoded
