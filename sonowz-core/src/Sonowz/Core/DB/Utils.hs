{-# LANGUAGE UndecidableInstances #-}
module Sonowz.Core.DB.Utils where

import Control.Arrow
import qualified Control.Exception.Safe as E
import Data.Aeson (FromJSON, ToJSON)
import Data.Profunctor (dimap)
import Data.Profunctor.Product.Default (Default(def))
import qualified Database.PostgreSQL.Simple.FromField as FF
import Opaleye
import qualified Opaleye.Aggregate as Agg
import Servant (ServerError(errBody), err500)
import Sonowz.Core.Imports hiding (null)
import Sonowz.Core.StdEff.Effect


newtype DatabaseException = DatabaseException Text deriving (Show, Exception)

newtype Uid = Uid Int
  deriving (Eq, Show, Read) deriving (Num, ToJSON, FromJSON) via Int
deriving via Int instance QueryRunnerColumnDefault SqlInt4 Uid
instance Default Constant Uid (Column SqlInt4) where
  def = coerce (def :: Constant Int (Column SqlInt4))

-- Default null value in aggregate operations
nullify :: Aggregator (Field a) (FieldNullable a)
nullify = dimap id (const null) Agg.count

putArg :: SelectArr a b -> a -> Select b
putArg f a = arr (const a) >>> f


-- Use this when instantiating 'FromField' typeclass
fromFieldSimple
  :: Typeable hasktype => (ByteString -> Either Text hasktype) -> FF.FieldParser hasktype
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
maybeToException actionText action = action >>= \case
  Nothing    -> throwException actionText
  Just value -> return value
