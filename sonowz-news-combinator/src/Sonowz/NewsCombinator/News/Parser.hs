module Sonowz.NewsCombinator.News.Parser
  ( parseNewsItems
  , ParseException
  ) where

import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Sonowz.NewsCombinator.Imports
import Sonowz.NewsCombinator.News.Types (NewsItem(..))
import Text.HTML.TagSoup (Tag(..), innerText, parseTags, partitions, renderTags, (~/=), (~==))

newtype ParseException = ParseException String deriving (Show, Exception)
type XMLParser a = [Tag Text] -> Either ParseException a


-- Exported function
-- parses XML body into NewsItem
parseNewsItems :: Text -> Either ParseException [NewsItem]
parseNewsItems = pNewsItems . parseTags

-- Type annotation
s :: String -> String
s = id

pNewsItems :: XMLParser [NewsItem]
pNewsItems = traverse pNewsItem . partitions (~== s "<item>")

pNewsItem :: XMLParser NewsItem
pNewsItem xml = NewsItem title <$> eitherDate <*> eitherAttrs where
  title = innerText . take 2 . dropWhile (~/= s "<title>") $ xml
  eitherDate :: Either ParseException UTCTime
  eitherDate = pGMTTime . innerText . take 2 . dropWhile (~/= s "<pubDate>") $ xml
  eitherAttrs :: Either ParseException (Map Text Text)
  eitherAttrs = fmap fromList . traverse pAttrs . partitions isAttr . drop 1 $ xml
  isAttr (TagOpen "title"       _) = True
  isAttr (TagOpen "link"        _) = True
  isAttr (TagOpen "guid"        _) = True
  isAttr (TagOpen "pubDate"     _) = True
  isAttr (TagOpen "description" _) = True
  isAttr (TagOpen "source"      _) = True
  isAttr _                         = False

-- (attr name, attr value)
pAttrs :: XMLParser (Text, Text)
pAttrs (TagOpen name _ : tags) = Right (name, value)
  where value = renderTags $ takeWhile (~/= s ("</" <> toString name <> ">")) tags
pAttrs xml = Left (ParseException $ "'pAttrs' failed: " <> toString (renderTags xml))

pGMTTime :: Text -> Either ParseException UTCTime
pGMTTime (toString -> text) = maybeToException msg mbParsed where
  msg      = "'pGMTTime' failed: " <> text
  mbParsed = parseTimeM True defaultTimeLocale "%a, %d %b %Y %T GMT" text

maybeToException :: String -> Maybe a -> Either ParseException a
maybeToException msg = maybeToRight (ParseException msg)
