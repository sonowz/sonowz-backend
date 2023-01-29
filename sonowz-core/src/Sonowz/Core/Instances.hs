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

instance Default ToFields NominalDiffTime (Field SqlFloat8) where
  def = toToFields (toFields . fromRational @Double . toRational)

-- TODO: delete if unnecessary
-- instance Default ToFields NominalDiffTime (FieldNullable SqlFloat8) where
--  def = toToFields (toFields . Just . fromRational @Double . toRational)

instance DefaultFromField SqlFloat8 NominalDiffTime where
  defaultFromField = fromPGSFromField

instance FF.FromField NominalDiffTime where
  fromField = fmap (fromRational . toRational) <<$>> (FF.fromField :: FF.FieldParser Double)
