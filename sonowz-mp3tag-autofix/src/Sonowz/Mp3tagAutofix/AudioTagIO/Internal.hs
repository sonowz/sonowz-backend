module Sonowz.Mp3tagAutofix.AudioTagIO.Internal
  ( audioTagGetter,
    audioTagSetter,
  )
where

import Data.Text qualified as T
import Data.Text.ICU.CharsetDetection (detect, getConfidence, getName)
import Data.Text.ICU.Convert (fromUnicode, open, toUnicode)
import Relude.Extra.Tuple (dup)
import Sonowz.Mp3tagAutofix.AudioTag.Types (AudioTag (..), Encoding (..))
import Sonowz.Mp3tagAutofix.Imports
import Sound.HTagLib
import System.IO.Unsafe (unsafePerformIO)

data WithEncoding a = WithUtf8 a | WithOther a deriving (Functor, Foldable, Traversable)

instance Applicative WithEncoding where
  -- If 'Other' encoding exists, it is priortized
  pure = WithUtf8
  (<*>) (WithUtf8 f) (WithUtf8 a) = WithUtf8 (f a)
  (<*>) (WithUtf8 f) (WithOther a) = WithOther (f a)
  (<*>) (WithOther f) (WithUtf8 a) = WithOther (f a)
  (<*>) (WithOther f) (WithOther a) = WithOther (f a)

audioTagGetter :: FilePath -> TagGetter AudioTag
audioTagGetter filename =
  fmap setEncoding $
    AudioTag EncodingUtf8 filename
      <<$>> withEncoding mkTitle (autofixEncoding . unTitle <$> titleGetter)
      <<*>> withEncoding mkArtist (autofixEncoding . unArtist <$> artistGetter)
      <<*>> withEncoding mkAlbum (autofixEncoding . unAlbum <$> albumGetter)
      <<*>> withEncoding mkComment (autofixEncoding . unComment <$> commentGetter)
      <<*>> withEncoding mkGenre (autofixEncoding . unGenre <$> genreGetter)
      <<*>> fmap pure yearGetter
      <<*>> fmap pure trackNumberGetter
  where
    infixl 4 <<*>>
    (<<*>>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
    (<<*>>) f = (<*>) (fmap (<*>) f)
    withEncoding :: (a -> b) -> TagGetter (Encoding, a) -> TagGetter (WithEncoding b)
    withEncoding f = fmap $ \(enc, a) -> case enc of
      EncodingUtf8 -> WithUtf8 (f a)
      EncodingOther -> WithOther (f a)
    setEncoding :: WithEncoding AudioTag -> AudioTag
    setEncoding (WithUtf8 tag) = tag
    setEncoding (WithOther tag) = tag {encoding = EncodingOther}

-- Only set fields which has been changed from original tag
-- Or if original encoding is not UTF-8, set all tags to UTF-8
type SetterFn = forall a. Eq a => (AudioTag -> a) -> (a -> TagSetter) -> TagSetter

audioTagSetter :: AudioTag -> AudioTag -> TagSetter
audioTagSetter orig target =
  setterFn title titleSetter
    <> setterFn artist artistSetter
    <> setterFn album albumSetter
    <> setterFn comment commentSetter
    <> setterFn genre genreSetter
    <> setterFn year yearSetter
    <> setterFn trackNumber trackNumberSetter
  where
    setterFn :: SetterFn
    setterFn = if encoding target == EncodingUtf8 then setterIfChanged else setterAlways
    setterAlways :: SetterFn
    setterAlways field setter = setter (field target)
    setterIfChanged :: SetterFn
    setterIfChanged field setter =
      if field orig /= field target then setter (field target) else mempty

-- Fix encodings where 'htaglib' fails to detect, especially 'CP949'
autofixEncoding :: HasCallStack => Text -> (Encoding, Text)
autofixEncoding text
  | isProperText text = (EncodingUtf8, text)
  | isProperText (latin1ToCP949 text) = (EncodingOther, latin1ToCP949 text)
  | isProperText (utf8ToCP949 text) = (EncodingOther, utf8ToCP949 text)
  | isProperText (utf8ToLatin1 text) = (EncodingOther, utf8ToLatin1 text)
  | otherwise = (EncodingOther, autoDetect text)
  where
    -- Tag words must start with [A-Za-z0-9가-힣]
    isProperText :: Text -> Bool
    isProperText = all go . words . T.filter (/= '(') . T.strip
      where
        go :: Text -> Bool
        go t = t == "" || ("0" <= t && t <= "z") || ("가" <= t && t <= "힣")
    converterCP949 = unsafePerformIO $ open "cp949" Nothing
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
      let encoded1 = fromUnicode converterLatin1 text
          encoded2 = encodeUtf8 text
          bitraverse' :: Applicative m => (a -> m b) -> (a -> m c) -> a -> m (b, c)
          bitraverse' f g = bitraverse f g . dup
      (score1, decoding1) <- bitraverse' getConfidence getName =<< detect encoded1
      (score2, decoding2) <- bitraverse' getConfidence getName =<< detect encoded2
      let (encoded, decoding) =
            if score1 >= score2 then (encoded1, decoding1) else (encoded2, decoding2)
      converter <- open (toString decoding) Nothing
      return $ toUnicode converter encoded
