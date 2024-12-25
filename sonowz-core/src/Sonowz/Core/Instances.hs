module Sonowz.Core.Instances where

import Data.Profunctor.Product.Default (Default (def))
import Data.Time (NominalDiffTime)
import Opaleye
  ( DefaultFromField (defaultFromField),
    Field,
    SqlFloat8,
    ToFields,
    toFields,
    toToFields,
    unsafeFromField,
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
  defaultFromField = unsafeFromField (fromRational . toRational) (defaultFromField @SqlFloat8 @Double)
