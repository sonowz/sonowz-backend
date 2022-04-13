module Sonowz.Mp3tagAutofix.AudioTag.Autofix.Parser
  ( parseSearchResultArtist
  , parseSearchResultSong
  , ParseException
  ) where

import Sonowz.Mp3tagAutofix.Imports

import Data.List (nub)
import qualified Data.Text as T
import Sonowz.Mp3tagAutofix.AudioTag.Autofix.Types
import Sonowz.Mp3tagAutofix.AudioTag.Types (Artist, Title, mkArtist, mkTitle)
import Text.HTML.TagSoup


newtype ParseException = ParseException String deriving (Show, Typeable)
instance Exception ParseException
type HTMLParser a = [Tag Text] -> Either ParseException a

parseSearchResultArtist :: Text -> Either ParseException SearchResultArtist
parseSearchResultArtist = pArtistSection . parseTags

parseSearchResultSong :: Text -> Either ParseException SearchResultSong
parseSearchResultSong = pSongSection . parseTags

-- Type annotation
s :: String -> String
s = id


pArtistSection :: HTMLParser SearchResultArtist
pArtistSection html = Right (SearchResultArtist artists)
 where
  sectionHtml =
    takeWhile (~/= s "<div class=section_song>")
      . dropWhile (~/= s "<div class=section_atist>") -- 'atist' is not a typo
      $ html
  exactMatch :: Maybe Artist
  exactMatch =
    fmap mkArtist . pText . drop 1 . dropWhile (~/= s "<strong class=fc_serch>") $ sectionHtml
  possibleMatches :: [Artist]
  possibleMatches =
    mapMaybe (fmap mkArtist . pText . drop 1 . dropWhile (~/= s "<a>") . dropWhile (~/= s "<dt>"))
      . partitions (~== s "<li>")
      . dropWhile (~/= s "<div class=dongname>")
      $ sectionHtml
  artists = maybeToList exactMatch <> possibleMatches


pSongSection :: HTMLParser SearchResultSong
pSongSection html = songs >>= Right . SearchResultSong
 where
  sectionHtml =
    takeWhile (~/= s "<div class=section_album>")
      . dropWhile (~/= s "<div class=section_song>")
      $ html
  totalSearchSection =
    find (\tags -> isJustTrue $ T.isInfixOf "전체에서 검색" <$> pText tags)
      . fmap (drop 1)
      . sections (~== s "<h4 class=\"title arr\">")
      $ sectionHtml
  titleSearchSection = sectionHtml
  songHtmlList =
    partitions (~== s "<tr>")
      .  takeWhile (~/= s "</tbody>")
      .  dropWhile (~/= s "<tbody>")
      $  totalSearchSection
      ?: titleSearchSection
  songs = catMaybes <$> traverse pSong songHtmlList
  isJustTrue (Just True) = True
  isJustTrue _           = False


pSong :: HTMLParser (Maybe Song)
pSong html = case title of
  Left  e        -> Left e
  Right Nothing  -> Right Nothing
  Right (Just t) -> Just . flip Song t <$> artists
 where
  title :: Either ParseException (Maybe Title)
  title =
    Right
      .   fmap mkTitle
      .   pText
      .   drop 1
      .   dropWhile (~/= s "<a class=fc_gray>")
      =<< nth 3 (isTagOpenName "td") html
  artists :: Either ParseException (NonEmpty Artist)
  artists = do
    linked  <- linkedArtists
    artists <- if null linked then plainArtists else pure linked
    checkNonEmpty "artist" artists
  linkedArtists :: Either ParseException [Artist]
  linkedArtists =
    Right
      .   ordNub
      .   mapMaybe (fmap (mkArtist . T.strip) . pText . drop 1)
      .   sections (~== s "<a class=fc_mgray>")
      .   takeWhile (not . isTagCloseName "td")
      =<< nth 4 (isTagOpenName "td") html
  plainArtists :: Either ParseException [Artist]
  plainArtists =
    Right
      .   ordNub
      .   mapMaybe (fmap (mkArtist . T.strip) . pText . drop 1)
      .   sections plainTextArtist
      .   takeWhile (not . isTagCloseName "td")
      =<< nth 4 (isTagOpenName "td") html
  plainTextArtist (TagOpen "div" (("id", "artistName") : _)) = True
  plainTextArtist _ = False
  nth :: Int -> (Tag Text -> Bool) -> HTMLParser [Tag Text]
  nth n selector = maybeToRight msg . (!!? (n - 1)) . sections selector
    where msg = ParseException $ "No " <> show n <> "th selector exist!"
  maybeToExc :: Text -> Maybe a -> Either ParseException a
  maybeToExc target = maybeToRight (ParseException $ "Failed to parse " <> toString target)
  checkNonEmpty :: Text -> [a] -> Either ParseException (NonEmpty a)
  checkNonEmpty target = maybeToExc target . nonEmpty

-- Extract head tag as innerText
-- handle cases where sometimes '<b>' exists
pText :: [Tag Text] -> Maybe Text
pText (TagOpen "b" [] : TagText text : TagClose "b" : tags) = case pText tags of
  Just text2 -> Just (text <> text2)
  Nothing    -> Just text
pText (TagText text : tags) = case pText tags of
  Just text2 -> Just (text <> text2)
  Nothing    -> Just text
pText _ = Nothing


