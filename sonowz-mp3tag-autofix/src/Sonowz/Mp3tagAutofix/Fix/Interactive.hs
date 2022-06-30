module Sonowz.Mp3tagAutofix.Fix.Interactive
  ( interactiveFilterFix,
  )
where

import Data.Text qualified as T
import Sonowz.Mp3tagAutofix.Fix.Types (Fix, listFixes, mkFix)
import Sonowz.Mp3tagAutofix.Imports

-- Note: this function includes interactive 'getLine' function
interactiveFilterFix ::
  forall a r.
  (Ord a, Members (Embed IO : StdEff) r, HasCallStack) =>
  Fix a ->
  (a -> Text) ->
  Sem r (Fix a)
interactiveFilterFix fixes showFn = constructFix <$> go (listFixes fixes)
  where
    go :: [(a, a)] -> Sem r [(a, a)]
    go fixesList = do
      logInfo "List of fixes:"
      logInfo (if null fixesList then " EMPTY!" else prettyPrintFixes fixesList)

      logInfo "Select indices of fix to exclude (leave empty to end selection):"
      indices <- parseIndices <$> liftIO getLine

      if null indices then return fixesList else go (removeByIndices indices fixesList)

    prettyPrintFixes :: [(a, a)] -> Text
    prettyPrintFixes fixesList =
      -- "1.    artist1 → artist2"
      fold $ zipWith zipFn [1 ..] $ (\(src, dst) -> showFn src <> " → " <> showFn dst) <$> fixesList
    parseIndices :: Text -> [Int]
    parseIndices = mapMaybe (rightToMaybe . readEither . toString) . words
    zipFn n fixText = "\n" <> T.justifyLeft 5 ' ' (show n <> ".") <> " " <> fixText
    removeByIndices :: [Int] -> [(a, a)] -> [(a, a)]
    removeByIndices indices =
      catMaybes . zipWith (\n x -> if n `elem` indices then Nothing else Just x) [1 ..]
    constructFix :: [(a, a)] -> Fix a
    constructFix = foldl' (<>) mempty . fmap mkFix
