module Sonowz.Mp3tagAutofix.AudioTag.Autofix.Parser
  ( parseSearchResultArtist
  , parseSearchResultSong
  , ParseException
  ) where

import Sonowz.Mp3tagAutofix.Imports

import qualified Data.Set as Set
import Sonowz.Mp3tagAutofix.AudioTag.Autofix.Types
import Sonowz.Mp3tagAutofix.AudioTag.Types (Artist, Title, mkArtist, mkTitle)
import Text.HTML.TagSoup


newtype ParseException = ParseException String deriving (Show, Exception)
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
  songHtmlList =
    partitions (~== s "<tr>")
      . takeWhile (~/= s "</tbody>")
      . dropWhile (~/= s "<tbody>")
      $ sectionHtml
  songs = traverse pSong songHtmlList


pSong :: HTMLParser Song
pSong html = Song <$> artists <*> title where
  title :: Either ParseException Title
  title =
    maybeToExc "title"
      .   fmap mkTitle
      .   pText
      .   drop 1
      .   dropWhile (~/= s "<a class=fc_gray>")
      =<< nth 3 (isTagOpenName "td") html
  artists :: Either ParseException (NonEmpty Artist)
  artists =
    checkNonEmpty "artist"
      .   mapMaybe (fmap mkArtist . pText . drop 1)
      .   sections (~== s "<a class=fc_mgray>")
      .   takeWhile (not . isTagCloseName "td")
      =<< nth 4 (isTagOpenName "td") html
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
pText (TagOpen "b" [] : TagText text : TagClose "b" : TagText text2 : _) = Just (text <> text2)
pText (TagOpen "b" [] : TagText text : TagClose "b" : _) = Just text
pText (TagText text : _) = Just text
pText _                  = Nothing


