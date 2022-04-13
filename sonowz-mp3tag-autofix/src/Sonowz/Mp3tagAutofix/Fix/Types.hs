module Sonowz.Mp3tagAutofix.Fix.Types
  ( Fix
  , mkFix
  , applyFix
  ) where

import Sonowz.Mp3tagAutofix.Imports

import Data.Map (assocs, lookup)
import qualified Text.Show as S


newtype Fix a = Fix (Map a a)
  deriving (Eq)
  deriving (Semigroup, Monoid) via (Map a a)

instance (Ord a, Show a) => S.Show (Fix a) where
  show (Fix m) = "[" <> intercalate "\n," (showPair <$> sortPair (assocs m)) <> "]"   where
    sortPair = sortBy (compare `on` snd)
    showPair :: (a, a) -> String
    showPair (src, dst) = show src <> " → " <> show dst

-- Constructor
mkFix :: Ord a => (a, a) -> Fix a
mkFix = Fix . fromList . one

-- Convert to function "a -> a"
applyFix :: (Ord a, Eq a) => Fix a -> a -> a
applyFix (Fix m) x = fromMaybe x (lookup x m)
