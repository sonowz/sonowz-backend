module Sonowz.Raytrace.DB.QueryUtil
  ( boolToException
  , maybeToException
  )
where

import UnliftIO.Exception (throwIO)

import Sonowz.Raytrace.Imports
import Sonowz.Raytrace.Monad.MQueue (MQueueException(..))

throwException text = throwIO (MQueueException $ "Error occurred while " <> text)

boolToException :: MonadIO m => Text -> m Bool -> m ()
boolToException actionText action = unlessM action (throwException actionText)

maybeToException :: MonadIO m => Text -> m (Maybe a) -> m a
maybeToException actionText action = action >>= \case
  Nothing -> throwException actionText
  Just value -> return value
