module Sonowz.Core.DB.Utils
  ( DatabaseException (..),
    nullify,
    putArg,
    fromFieldSimple,
    maybeToExceptionIO,
    throwException,
    boolToException,
    maybeToException,
    updateAlternative,
    AlternativeUpdater,
  )
where

import Control.Arrow (Arrow (arr, (***)))
import Control.Exception.Safe qualified as E
import Data.Profunctor (Profunctor, dimap)
import Data.Profunctor.Product (ProductProfunctor (..))
import Data.Profunctor.Product.Default (Default (def))
import Database.PostgreSQL.Simple.FromField qualified as FF
import Opaleye
import Opaleye.Aggregate qualified as Agg
import Opaleye.Internal.Manipulation (Updater (..))
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

-- Use this in 'Update.uUpdateWith'
-- This ensures that 'Nothing' fields in 'writeFields' won't be updated to the database
updateAlternative :: (Default Updater r w, Default AlternativeUpdater w w) => w -> r -> w
updateAlternative writeFields origFields = writeFields <|> toWriteFields origFields
  where
    Updater toWriteFields = def
    (<|>) = (curry . getUpdater) def

newtype AlternativeUpdater a b = AlternativeUpdater {getUpdater :: (a, a) -> b}

instance Functor (AlternativeUpdater a) where
  fmap f (AlternativeUpdater g) = AlternativeUpdater (fmap f g)

instance Applicative (AlternativeUpdater a) where
  pure = AlternativeUpdater . pure
  AlternativeUpdater f <*> AlternativeUpdater x = AlternativeUpdater (f <*> x)

instance Profunctor AlternativeUpdater where
  dimap f g (AlternativeUpdater h) = AlternativeUpdater (dimap (f *** f) g h)

instance ProductProfunctor AlternativeUpdater where
  purePP = pure
  (****) = (<*>)

instance Default AlternativeUpdater (Maybe (Field_ n col)) (Maybe (Field_ n col)) where
  def = AlternativeUpdater (uncurry (<|>))

instance Default AlternativeUpdater (Field_ n col) (Field_ n col) where
  def = AlternativeUpdater fst
