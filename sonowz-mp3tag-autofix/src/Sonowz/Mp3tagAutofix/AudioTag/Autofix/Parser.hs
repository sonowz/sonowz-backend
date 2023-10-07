module Sonowz.Mp3tagAutofix.AudioTag.Autofix.Parser
  ( parseSearchResultArtist,
    parseSearchResultSong,
  )
where

import Data.Text qualified as T
import Sonowz.Core.Exception.Types (ParseException (..))
import Sonowz.Mp3tagAutofix.AudioTag.Autofix.Types
import Sonowz.Mp3tagAutofix.AudioTag.Types (Artist, Title, mkArtist, mkTitle)
import Sonowz.Mp3tagAutofix.Imports
import Text.HTML.TagSoup

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
    sectionHtml = dropWhile (~/= s "<div id=pageList>") html -- 'atist' is not a typo
    artists :: [Artist]
    artists =
      dropWhile (~/= s "<ul>")
        >>> takeWhile (~/= s "</ul>")
        >>> partitions (~== s "<li>")
        >>> mapMaybe
          (fmap mkArtist . pText . drop 1 . dropWhile (~/= s "<a>") . dropWhile (~/= s "<dt>"))
        >>> take 4 -- Too many artists are useless
        $ sectionHtml

pSongSection :: HTMLParser SearchResultSong
pSongSection html = songs >>= Right . SearchResultSong
  where
    sectionHtml =
      dropWhile (isTagOpenName "table") >>> takeWhile (not . isTagCloseName "table") $ html
    songHtmlList =
      dropWhile (~/= s "<tbody>")
        >>> takeWhile (~/= s "</tbody>")
        >>> partitions (~== s "<tr>")
        $ sectionHtml
    -- 10 entries are sufficient
    songs = take 10 . catMaybes <$> traverse pSong songHtmlList

pSong :: HTMLParser (Maybe Song)
pSong html = case title of
  Left e -> Left e
  Right Nothing -> Right Nothing
  Right (Just t) -> Just . flip Song t <$> artists
  where
    title :: Either ParseException (Maybe Title)
    title =
      nth 3 (isTagOpenName "td") html
        >>= (dropWhile (~/= s "<a class=fc_gray>") >>> drop 1 >>> pText >>> fmap mkTitle >>> Right)
    artists :: Either ParseException (NonEmpty Artist)
    artists = do
      linked <- linkedArtists
      artists <- if null linked then plainArtists else pure linked
      checkNonEmpty "artist" artists
    linkedArtists :: Either ParseException [Artist]
    linkedArtists =
      nth 4 (isTagOpenName "td") html
        >>= ( takeWhile (not . isTagCloseName "td")
                >>> sections (~== s "<a class=fc_mgray>")
                >>> mapMaybe (fmap (mkArtist . T.strip) . pText . drop 1)
                >>> ordNub
                >>> Right
            )
    plainArtists :: Either ParseException [Artist]
    plainArtists =
      nth 4 (isTagOpenName "td") html
        >>= ( takeWhile (not . isTagCloseName "td")
                >>> sections plainTextArtist
                >>> mapMaybe (fmap (mkArtist . T.strip) . pText . drop 1)
                >>> ordNub
                >>> Right
            )
    plainTextArtist (TagOpen "div" (("id", "artistName") : _)) = True
    plainTextArtist _ = False
    nth :: Int -> (Tag Text -> Bool) -> HTMLParser [Tag Text]
    nth n selector = maybeToRight msg . (!!? (n - 1)) . sections selector
      where
        msg = ParseException $ "No " <> show n <> "th selector exist!"
    maybeToExc :: Text -> Maybe a -> Either ParseException a
    maybeToExc target = maybeToRight (ParseException $ "Failed to parse " <> target)
    checkNonEmpty :: Text -> [a] -> Either ParseException (NonEmpty a)
    checkNonEmpty target = maybeToExc target . nonEmpty

-- Extract head tag as innerText
-- handle cases where sometimes '<b>' exists
pText :: [Tag Text] -> Maybe Text
pText (TagOpen "b" [] : TagText text : TagClose "b" : tags) = case pText tags of
  Just text2 -> Just (text <> text2)
  Nothing -> Just text
pText (TagText text : tags) = case pText tags of
  Just text2 -> Just (text <> text2)
  Nothing -> Just text
pText _ = Nothing
