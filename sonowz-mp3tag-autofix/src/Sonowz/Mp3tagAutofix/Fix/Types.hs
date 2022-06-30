module Sonowz.Mp3tagAutofix.Fix.Types
  ( Fix,
    mkFix,
    applyFix,
    normalize,
    listFixes,
  )
where

import Data.Map.Strict qualified as M
import Relude.Extra.Newtype (un)
import Sonowz.Mp3tagAutofix.Imports
import Text.Show qualified as S

newtype Fix a = Fix (Map a a)
  deriving (Eq)
  deriving (Semigroup, Monoid) via (Map a a)

instance (Ord a, Show a) => S.Show (Fix a) where
  show (Fix m) = "[" <> intercalate "\n," (showPair <$> sortFixPair (M.assocs m)) <> "]"
    where
      showPair :: (a, a) -> String
      showPair (src, dst) = show src <> " → " <> show dst

-- Constructor
mkFix :: Ord a => (a, a) -> Fix a
mkFix = Fix . fromList . one

-- Convert to function "a -> a"
applyFix :: (Ord a, Eq a) => Fix a -> a -> a
applyFix (Fix m) x = fromMaybe x (M.lookup x m)

-- Remove 'Identity' fix ("x" -> "x")
normalize :: Eq a => Fix a -> Fix a
normalize (Fix m) = Fix (M.filterWithKey (/=) m)

listFixes :: (Eq a, Ord a) => Fix a -> [(a, a)]
listFixes = sortFixPair . M.assocs . un . normalize

sortFixPair :: Ord a => [(a, a)] -> [(a, a)]
sortFixPair = sortBy (compare `on` snd)
