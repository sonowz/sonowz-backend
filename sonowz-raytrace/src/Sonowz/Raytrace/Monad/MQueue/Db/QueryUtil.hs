module Sonowz.Raytrace.Monad.MQueue.Db.QueryUtil where

import Relude
import UnliftIO.Exception (throwIO)
import qualified Database.PostgreSQL.Simple as PGS

import Sonowz.Raytrace.Core.Has (grab)
import Sonowz.Raytrace.Monad.MQueue (WithDb, MQueueException(..))

grabConn :: WithDb m => m PGS.Connection
grabConn = grab

throwException text = throwIO (MQueueException $ "Error occurred while" <> text)

boolToException :: MonadIO m => Text -> m Bool -> m ()
boolToException actionText action = unlessM action (throwException actionText)

maybeToException :: MonadIO m => Text -> m (Maybe a) -> m a
maybeToException actionText action = fromMaybe <$> throwException actionText <*> action
