{-# LANGUAGE UndecidableInstances #-}

module Sonowz.Core.DB.Utils where

import Control.Arrow
import Control.Exception.Safe qualified as E
import Data.Profunctor (dimap)
import Database.PostgreSQL.Simple.FromField qualified as FF
import Opaleye
import Opaleye.Aggregate qualified as Agg
import Sonowz.Core.Imports hiding (null)
import Sonowz.Core.StdEff.Effect

newtype DatabaseException = DatabaseException Text
  deriving (Show)
  deriving anyclass (Exception)

-- Default null value in aggregate operations
nullify :: Aggregator (Field a) (FieldNullable a)
nullify = dimap id (const null) Agg.count

putArg :: SelectArr a b -> a -> Select b
putArg f a = arr (const a) >>> f

-- Use this when instantiating 'FromField' typeclass
fromFieldSimple ::
  Typeable hasktype => (ByteString -> Either Text hasktype) -> FF.FieldParser hasktype
fromFieldSimple parse field = \case
  Just bs -> either (FF.returnError FF.ConversionFailed field . toString) return (parse bs)
  Nothing -> FF.returnError FF.UnexpectedNull field "Unexpected null value"

maybeToExceptionIO :: HasCallStack => Text -> Maybe a -> IO a
maybeToExceptionIO msg = withFrozenCallStack $ maybe (E.throw $ DatabaseException msg) return

throwException :: Members StdEff r => Text -> Sem r a
throwException text = throw' (DatabaseException $ "Error occurred while " <> text)

boolToException :: Members StdEff r => Text -> Sem r Bool -> Sem r ()
boolToException actionText action = unlessM action (throwException actionText)

maybeToException :: Members StdEff r => Text -> Sem r (Maybe a) -> Sem r a
maybeToException actionText action =
  action >>= \case
    Nothing -> throwException actionText
    Just value -> return value
