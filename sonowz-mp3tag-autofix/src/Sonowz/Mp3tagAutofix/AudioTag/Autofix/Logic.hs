module Sonowz.Mp3tagAutofix.AudioTag.Autofix.Logic
  ( makeArtistPool
  , runSearches
  , makeArtistFixes
  ) where

import Sonowz.Mp3tagAutofix.Imports

import qualified Data.List as L
import qualified Data.Map as M
import Relude.Extra.Newtype (un)
import Relude.Extra.Tuple (fmapToFst, traverseToSnd)
import qualified Relude.Unsafe as Unsafe
import Sonowz.Core.HTTP.Effect (HTTP, fetchURL)
import Sonowz.Core.Time.Effect (Time, threadDelay)
import Sonowz.Mp3tagAutofix.AudioTag.Autofix.Parser
  (ParseException, parseSearchResultArtist, parseSearchResultSong)
import Sonowz.Mp3tagAutofix.AudioTag.Autofix.Types
  ( ArtistPool
  , ArtistPoolWithSearchResult
  , SearchResult(..)
  , SearchResultArtist(..)
  , SearchResultSong(..)
  , Song(..)
  , melonSearchUrl
  )
import Sonowz.Mp3tagAutofix.AudioTag.Types (Artist, AudioTag(..), joinArtistList, unArtist, unTitle)
import Sonowz.Mp3tagAutofix.Fix.Types (Fix, mkFix)


newtype LogicError = LogicError String deriving (Show, Exception)
type LogicM = Either LogicError


makeArtistPool :: [AudioTag] -> ArtistPool
makeArtistPool = fromList . fmapToFst artist


runSearches
  :: forall r
   . Members '[Time , HTTP , Error ParseException] r
  => ArtistPool
  -> Sem r ArtistPoolWithSearchResult
-- First 'traverse': Map
-- Second 'traverseToSnd': Tuple
runSearches = traverse $ traverseToSnd action where
  action :: AudioTag -> Sem r SearchResult
  action AudioTag {..} = do
    artistSearchBody <- fetchURL . melonSearchUrl . unArtist $ artist
    titleSearchBody  <- fetchURL . melonSearchUrl . unTitle $ title
    threadDelay (10 ^ 6) -- Sleep for 1 second
    fromEither
      $   SearchResult
      <$> parseSearchResultArtist artistSearchBody
      <*> parseSearchResultSong titleSearchBody


makeArtistFixes :: ArtistPoolWithSearchResult -> LogicM (Fix Artist)
makeArtistFixes = fmap concatFix . mapM elemMapFn where
  elemMapFn :: (AudioTag, SearchResult) -> LogicM Artist
  elemMapFn = uncurry calcBestMatchArtist
  concatFix :: Map Artist Artist -> Fix Artist
  concatFix = foldMap mkFix . M.assocs


data SongRank = SongRank
  { score :: Double
  , song  :: Song
  }
  deriving (Show, Eq, Ord)


-- Main Logic
calcBestMatchArtist :: AudioTag -> SearchResult -> LogicM Artist
calcBestMatchArtist tag search = picked where
  -- 1. Songs are ranked by using similarity score with original tag
  songs     = un $ songSection search
  artists   = un $ artistSection search :: [Artist]
  tagTitle  = unTitle $ title tag :: Text
  tagArtist = unArtist $ artist tag :: Text
  getSongTitle (Song _ t) = unTitle t :: Text
  songRanks :: [SongRank]
  songRanks = (\song -> SongRank (similarity tagTitle $ getSongTitle song) song) <$> songs

  -- 2. Reflect that the web search list is ordered by relevance
  magSongFn :: [SongRank -> SongRank]
  magSongFn  = [ \sr -> sr { score = score sr * m } | m <- (\x -> 1 - 0.02 * x) <$> [0 ..] ]
  songRanks' = zipWith ($) magSongFn songRanks

  -- 3. If a song's artist is in 'SearchResultArtist', double the score
  getSongArtist (Song a _) = a
  doubleFn sr = if not (null artistIntersection) then sr { score = 2 * score sr } else sr
    where artistIntersection = artists `L.intersect` toList (getSongArtist $ song sr)
  songRanks''  = doubleFn <$> songRanks'

  -- 4. Sort by score, and narrow down to 2 candidates
  sortDesc     = sortBy (flip compare)
  candidates   = take 2 $ sortDesc songRanks'' :: [SongRank]

  -- 5. If the first is sufficiently higher than the second (> 0.3), pick it
  --    Otherwise, pick by similiarity of artist with original tag
  returnArtist = Right . joinArtistList . getSongArtist . song :: SongRank -> LogicM Artist
  picked       = case candidates of
    []            -> Left $ LogicError "No best match!"
    [s1         ] -> returnArtist s1
    (s1 : s2 : _) -> returnArtist $ compareTwo s1 s2
  compareTwo s1 s2
    | score s1 > score s2 + 0.3 = s1
    | score s2 > score s1 + 0.3 = s2
    | similarityResult == LT    = s1
    | similarityResult == GT    = s2
    | otherwise                 = error "Same similarity between two candidates!"
   where
    similarityResult = (compare `on` getSimilarity) s1 s2
    getSimilarity    = similarity tagArtist . unArtist . joinArtistList . getSongArtist . song


-- String similarity, domain: [0.0, 1.0]
similarity :: Text -> Text -> Double
similarity (toString -> t1) (toString -> t2) = 1 - fromIntegral distance / fromIntegral maxLength where
  distance  = editDistance t1 t2
  maxLength = max (length t1) (length t2)


-- Code from: https://wiki.haskell.org/Edit_distance
editDistance :: Eq a => [a] -> [a] -> Int
editDistance a b = Unsafe.last
  (if lab == 0
    then mainDiag
    else if lab > 0
      then lowers Unsafe.!! (lab - 1)
      else{- < 0 -}
           uppers Unsafe.!! (-1 - lab)
  )
 where
  mainDiag = oneDiag a b (Unsafe.head uppers) (-1 : Unsafe.head lowers)
  uppers   = eachDiag a b (mainDiag : uppers) -- upper diagonals
  lowers   = eachDiag b a (mainDiag : lowers) -- lower diagonals
  eachDiag _ []       _                  = []
  eachDiag a (_ : bs) (lastDiag : diags) = oneDiag a bs nextDiag lastDiag : eachDiag a bs diags
    where nextDiag = Unsafe.head (Unsafe.tail diags)
  eachDiag _ _ _ = error "Unexpected error"
  oneDiag a b diagAbove diagBelow = thisdiag
   where
    doDiag []         _          _  _ _ = []
    doDiag _          []         _  _ _ = []
    doDiag (ach : as) (bch : bs) nw n w = me : doDiag as bs me (Unsafe.tail n) (Unsafe.tail w)
      where me = if ach == bch then nw else 1 + min3 (Unsafe.head w) nw (Unsafe.head n)
    firstelt = 1 + Unsafe.head diagBelow
    thisdiag = firstelt : doDiag a b firstelt diagAbove (Unsafe.tail diagBelow)
  lab = length a - length b
  min3 x y z = if x < y then x else min y z
