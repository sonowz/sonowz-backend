module Sonowz.Core.Instances where

import Servant (PlainText)
import Servant.API (ToHttpApiData(..), FromHttpApiData(..), MimeRender(..))
import URI.ByteString (URI, parseURI, laxURIParserOptions)

import Sonowz.Core.Imports


instance ToHttpApiData URI where
  toUrlPiece = show

instance FromHttpApiData URI where
  parseUrlPiece = first show . parseURI laxURIParserOptions . encodeUtf8

instance MimeRender PlainText URI where
  mimeRender _ = show
