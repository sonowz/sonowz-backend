module Sonowz.Raytrace.Monad.MQueue.Db.QueryUtil where

import Relude
import UnliftIO.Exception (throwIO)

import Sonowz.Raytrace.Core.Has (grab)
import Sonowz.Raytrace.Core.DB (DBConnPool)
import Sonowz.Raytrace.Monad.MQueue (WithDb, MQueueException(..))

grabPool :: WithDb m => m DBConnPool
grabPool = grab

throwException text = throwIO (MQueueException $ "Error occurred while " <> text)

boolToException :: MonadIO m => Text -> m Bool -> m ()
boolToException actionText action = unlessM action (throwException actionText)

maybeToException :: MonadIO m => Text -> m (Maybe a) -> m a
maybeToException actionText action = action >>= \case
  Nothing -> throwException actionText
  Just value -> return value
