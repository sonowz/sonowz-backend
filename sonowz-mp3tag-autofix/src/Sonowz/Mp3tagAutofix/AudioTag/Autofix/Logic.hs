module Sonowz.Mp3tagAutofix.AudioTag.Autofix.Logic
  ( makeArtistPool,
    runSearches,
    makeArtistFixes,
  )
where

import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Relude.Extra.Newtype (un)
import Relude.Extra.Tuple (fmapToFst)
import Relude.Unsafe qualified as Unsafe
import Sonowz.Core.Exception.Types (ParseException (..))
import Sonowz.Core.HTTP.Effect (HTTP, fetchURL)
import Sonowz.Core.Time.Effect (Time, threadDelay)
import Sonowz.Mp3tagAutofix.AudioTag.Autofix.Parser
  ( parseSearchResultArtist,
    parseSearchResultSong,
  )
import Sonowz.Mp3tagAutofix.AudioTag.Autofix.Types
  ( ArtistPool,
    ArtistPoolWithSearchResult,
    MelonSearchType (..),
    SearchResult (..),
    SearchResultArtist (..),
    SearchResultSong (..),
    Song (..),
    melonSearchUrl,
  )
import Sonowz.Mp3tagAutofix.AudioTag.Types
  ( Artist,
    AudioTag (..),
    artistList,
    joinArtistList,
    mkArtist,
    unArtist,
    unTitle,
  )
import Sonowz.Mp3tagAutofix.Fix.Types (Fix, mkFix)
import Sonowz.Mp3tagAutofix.Imports

makeArtistPool :: [AudioTag] -> ArtistPool
makeArtistPool = groupByArtist . fmapToFst artist
  where
    groupByArtist :: [(Artist, AudioTag)] -> Map Artist (NonEmpty AudioTag)
    groupByArtist = foldr (\(k, v) -> M.insertWith (<>) k (one v)) mempty

runSearches ::
  forall r.
  (Members '[Time, HTTP, Error ParseException, StdLog] r, HasCallStack) =>
  ArtistPool ->
  Sem r ArtistPoolWithSearchResult
-- 'traverse': Map
runSearches = M.traverseMaybeWithKey action
  where
    action :: Artist -> NonEmpty AudioTag -> Sem r (Maybe (AudioTag, SearchResult))
    action k (tag :| tags) = do
      let artistText = unArtist (artist tag)
          titleText = unTitle (title tag)
          -- These characters seems to corrupt title search results
          removeSpecialChars :: Text -> Text
          removeSpecialChars = T.replace "," "" . T.replace "." " "
          titleKeyword = removeSpecialChars titleText <> " " <> artistText
          plainTitleKeyword = removeSpecialChars titleText
          artistKeyword = artistText

      logDebug $ "Searching with keyword \"" <> plainTitleKeyword <> "\""
      titleSearchBody' <- fetchURL (melonSearchUrl MelonTitle plainTitleKeyword)
      searchResultSong' <- fromEither $ parseSearchResultSong titleSearchBody'

      if not $ null (un @[Song] searchResultSong')
        then do
          logDebug $ "Searching with keyword \"" <> titleKeyword <> "\""
          titleSearchBody <- fetchURL (melonSearchUrl MelonAll titleKeyword)
          searchResultSong <- fromEither $ parseSearchResultSong titleSearchBody

          logDebug $ "Searching with keyword \"" <> artistKeyword <> "\""
          artistSearchBody <- fetchURL (melonSearchUrl MelonArtist artistKeyword)
          searchResultArtist <- fromEither $ parseSearchResultArtist artistSearchBody

          threadDelay (10 ^ 6) -- Sleep for 1 second
          return $ Just (tag, SearchResult searchResultArtist (searchResultSong <> searchResultSong'))
        else do
          logWarning $ "No song named " <> titleText <> "."

          case nonEmpty tags of
            Nothing -> do
              let fileInfo =
                    artistText <> " - " <> titleText <> " (filename: " <> toText (filename tag) <> ")"
              logError ("\"" <> fileInfo <> "\" will be skipped.")
              return Nothing
            Just tags' -> action k tags'

makeArtistFixes :: ArtistPoolWithSearchResult -> Fix Artist
makeArtistFixes = concatFix . fmap elemMapFn
  where
    elemMapFn :: (AudioTag, SearchResult) -> Artist
    elemMapFn = uncurry calcBestMatchArtist
    concatFix :: Map Artist Artist -> Fix Artist
    concatFix = foldMap mkFix . M.assocs

data SongRank = SongRank
  { score :: Double,
    song :: Song
  }
  deriving (Show, Eq, Ord)

-- Main Logic
calcBestMatchArtist :: HasCallStack => AudioTag -> SearchResult -> Artist
calcBestMatchArtist tag search = picked
  where
    -- 1. Song titles are ranked by using similarity score with original tag
    songs = un $ songSection search
    artists = un $ artistSection search :: [Artist]
    tagTitle = unTitle $ title tag :: Text
    tagArtist = artistList $ artist tag :: NonEmpty Artist
    getSongTitle (Song _ t) = unTitle t :: Text
    songRanks :: [SongRank]
    songRanks = (\song -> SongRank (similarity tagTitle $ getSongTitle song) song) <$> songs

    -- 2. Reflect that the web search list is ordered by relevance
    magSongFn :: [SongRank -> SongRank]
    magSongFn = [\sr -> sr {score = score sr * m} | m <- (\x -> 1 - 0.02 * x) <$> [0 ..]]
    songRanks' = zipWith ($) magSongFn songRanks

    -- 3. "Various Artists" artist is discouraged (score = 0.1)
    filterFn sr =
      if getSongArtist (song sr) == [mkArtist "Various Artists"] then sr {score = 0.1} else sr
    songRanks'' = filterFn <$> songRanks'

    -- 4. Song artists are ranked by using similarity score with original tag
    getSongArtist (Song a _) = a
    plusFn sr = sr {score = score sr + artistSimilarity tagArtist (getSongArtist $ song sr)}
    songRanks''' = plusFn <$> songRanks''

    -- 5. If a song's artist is in 'SearchResultArtist', double the score
    doubleFn sr = if not (null artistIntersection) then sr {score = 2 * score sr} else sr
      where
        artistIntersection = artists `L.intersect` toList (getSongArtist $ song sr)
    songRanks'''' = doubleFn <$> songRanks'''

    -- 6. Sort by score, and narrow down to 2 candidates
    sortDesc = sortBy (flip compare)
    candidates = take 2 $ sortDesc songRanks'''' :: [SongRank]

    -- 7. If the first is sufficiently higher than the second (> 0.3), pick it
    --    Otherwise, pick by similiarity of artist with original tag
    returnArtist = joinArtistList . getSongArtist . song :: SongRank -> Artist
    picked = case candidates of
      [] -> error "No best match!"
      [s1] -> returnArtist s1
      (s1 : s2 : _) -> returnArtist $ compareTwo s1 s2
    compareTwo s1 s2
      | score s1 > score s2 + 0.3 = s1
      | score s2 > score s1 + 0.3 = s2
      | similarityResult == LT = s1
      | similarityResult == EQ = s1
      | similarityResult == GT = s2
      | otherwise = error "Unexpected code flow!"
      where
        similarityResult = (compare `on` getSimilarity) s1 s2
        getSimilarity = artistSimilarity tagArtist . getSongArtist . song

-- TODO: use max flow algorithm
-- Domain: [0.0, inf) (mostly [0.0, 1.0])
artistSimilarity :: NonEmpty Artist -> NonEmpty Artist -> Double
artistSimilarity l1 l2 = sum scores / fromIntegral (max (length l1) (length l2))
  where
    scores = [scoreFn a1 a2 | a1 <- toList l1, a2 <- toList l2]
    scoreFn (unArtist -> a1) (unArtist -> a2) = substrScore + similarityScore
      where
        -- Substring weighs cases like "A" ~= "A (feat. B)"
        substrScore = if T.isInfixOf a1 a2 || T.isInfixOf a2 a1 then 0.5 else 0
        similarityScore = 0.5 * similarity a1 a2

-- String similarity, domain: [0.0, 1.0]
similarity :: Text -> Text -> Double
similarity (toString -> t1) (toString -> t2) = 1 - fromIntegral distance / fromIntegral maxLength
  where
    distance = editDistance t1 t2
    maxLength = max (length t1) (length t2)

-- Code from: https://wiki.haskell.org/Edit_distance
editDistance :: Eq a => [a] -> [a] -> Int
editDistance a b =
  Unsafe.last
    ( if lab == 0
        then mainDiag
        else
          if lab > 0
            then lowers Unsafe.!! (lab - 1)
            else {- < 0 -}
              uppers Unsafe.!! (-1 - lab)
    )
  where
    mainDiag = oneDiag a b (Unsafe.head uppers) (-1 : Unsafe.head lowers)
    uppers = eachDiag a b (mainDiag : uppers) -- upper diagonals
    lowers = eachDiag b a (mainDiag : lowers) -- lower diagonals
    eachDiag _ [] _ = []
    eachDiag a (_ : bs) (lastDiag : diags) = oneDiag a bs nextDiag lastDiag : eachDiag a bs diags
      where
        nextDiag = Unsafe.head (Unsafe.tail diags)
    eachDiag _ _ _ = error "Unexpected error"
    oneDiag a b diagAbove diagBelow = thisdiag
      where
        doDiag [] _ _ _ _ = []
        doDiag _ [] _ _ _ = []
        doDiag (ach : as) (bch : bs) nw n w = me : doDiag as bs me (Unsafe.tail n) (Unsafe.tail w)
          where
            me = if ach == bch then nw else 1 + min3 (Unsafe.head w) nw (Unsafe.head n)
        firstelt = 1 + Unsafe.head diagBelow
        thisdiag = firstelt : doDiag a b firstelt diagAbove (Unsafe.tail diagBelow)
    lab = length a - length b
    min3 x y z = if x < y then x else min y z
