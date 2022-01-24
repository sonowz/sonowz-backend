module Sonowz.Core.Instances where

import Servant (PlainText)
import Servant.API (FromHttpApiData(..), MimeRender(..), ToHttpApiData(..))
import URI.ByteString (URI, laxURIParserOptions, parseURI, serializeURIRef')

import Data.Profunctor.Product.Default (Default(def))
import Opaleye (Column, Constant(..))
import Sonowz.Core.Imports


instance ToHttpApiData URI where
  toUrlPiece = decodeUtf8 . serializeURIRef'

instance FromHttpApiData URI where
  parseUrlPiece = first show . parseURI laxURIParserOptions . encodeUtf8

instance MimeRender PlainText URI where
  mimeRender _ = toLazy . serializeURIRef'

-- This instance is used in DB type declaration
-- where type of 'write field' is 'Maybe (Column col)',
-- to indicate that the field is not used in writes.
instance Default Constant a (Maybe (Column col)) where
  def = Constant (const Nothing)
