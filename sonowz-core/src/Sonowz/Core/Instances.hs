module Sonowz.Core.Instances where

import Data.Profunctor.Product.Default (Default (def))
import Data.Time (NominalDiffTime)
import Database.PostgreSQL.Simple.FromField qualified as FF
import Opaleye
  ( DefaultFromField (defaultFromField),
    Field,
    Field_,
    SqlFloat8,
    ToFields,
    fromPGSFromField,
    toFields,
    toToFields,
  )
import Servant (PlainText)
import Servant.API (FromHttpApiData (..), MimeRender (..), ToHttpApiData (..))
import Sonowz.Core.Imports
import URI.ByteString (URI, laxURIParserOptions, parseURI, serializeURIRef')

instance ToHttpApiData URI where
  toUrlPiece = decodeUtf8 . serializeURIRef'

instance FromHttpApiData URI where
  parseUrlPiece = first show . parseURI laxURIParserOptions . encodeUtf8

instance MimeRender PlainText URI where
  mimeRender _ = toLazy . serializeURIRef'

-- This instance is used in DB type declaration
-- where type of 'write field' is 'Maybe (Column col)',
-- to indicate that the field is not used in writes.
instance Default ToFields a (Maybe (Field_ n col)) where
  def = toToFields (const Nothing)

instance Default ToFields NominalDiffTime (Field SqlFloat8) where
  def = toToFields (toFields . fromRational @Double . toRational)

-- TODO: delete if unnecessary
-- instance Default ToFields NominalDiffTime (FieldNullable SqlFloat8) where
--  def = toToFields (toFields . Just . fromRational @Double . toRational)

instance DefaultFromField SqlFloat8 NominalDiffTime where
  defaultFromField = fromPGSFromField

instance FF.FromField NominalDiffTime where
  fromField = fmap (fromRational . toRational) <<$>> (FF.fromField :: FF.FieldParser Double)
