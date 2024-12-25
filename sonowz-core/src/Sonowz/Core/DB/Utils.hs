module Sonowz.Core.DB.Utils
  ( DatabaseException (..),
    nullify,
    putArg,
    fieldParserByReadInstance,
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

newtype DatabaseException = DatabaseException Text
  deriving (Show)
  deriving anyclass (Exception)

-- Default null value in aggregate operations
nullify :: Aggregator (Field a) (FieldNullable a)
nullify = dimap id (const null) Agg.count

putArg :: SelectArr a b -> a -> Select b
putArg f a = arr (const a) >>> f

fieldParserByReadInstance :: (Typeable a, Read a) => String -> FF.FieldParser a
fieldParserByReadInstance name field (Just bs) =
  either
    (const $ FF.returnError FF.ConversionFailed field ("Parse failed in: " <> name))
    return
    (readEither . toString $ decodeUtf8 @Text bs)
fieldParserByReadInstance name field Nothing = FF.returnError FF.UnexpectedNull field ("Null value in: " <> name)

maybeToExceptionIO :: HasCallStack => Text -> Maybe a -> IO a
maybeToExceptionIO msg = withFrozenCallStack $ maybe (E.throw $ DatabaseException msg) return

throwException :: Member (Embed IO) r => Text -> Sem r a
throwException text = liftIO $ E.throw (DatabaseException $ "Error occurred while " <> text)

boolToException :: Member (Embed IO) r => Text -> Sem r Bool -> Sem r ()
boolToException actionText action = unlessM action (throwException actionText)

maybeToException :: Member (Embed IO) r => Text -> Sem r (Maybe a) -> Sem r a
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
