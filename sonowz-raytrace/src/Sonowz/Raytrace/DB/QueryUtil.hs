module Sonowz.Raytrace.DB.QueryUtil
  ( boolToException
  , maybeToException
  )
where

import Sonowz.Raytrace.Imports
import Sonowz.Raytrace.DB.Types (DatabaseException(..))

throwException :: Members StdEff r => Text -> Sem r a
throwException text = throw' (DatabaseException $ "Error occurred while " <> text)

boolToException :: Members StdEff r => Text -> Sem r Bool -> Sem r ()
boolToException actionText action = unlessM action (throwException actionText)

maybeToException :: Members StdEff r => Text -> Sem r (Maybe a) -> Sem r a
maybeToException actionText action = action >>= \case
  Nothing -> throwException actionText
  Just value -> return value
