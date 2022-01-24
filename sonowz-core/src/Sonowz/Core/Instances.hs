module Sonowz.Core.Instances where

import Data.Profunctor.Product.Default (Default(def))
import Data.Time (NominalDiffTime)
import qualified Database.PostgreSQL.Simple.FromField as FF
import Opaleye
  ( Column
  , Constant(..)
  , QueryRunnerColumnDefault(defaultFromField)
  , SqlFloat8
  , ToFields
  , fieldQueryRunnerColumn
  , toFields
  , toToFields
  )
import Servant (PlainText)
import Servant.API (FromHttpApiData(..), MimeRender(..), ToHttpApiData(..))
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
instance Default Constant a (Maybe (Column col)) where
  def = Constant (const Nothing)

instance Default ToFields NominalDiffTime (Column SqlFloat8) where
  def = toToFields (toFields . fromRational @Double . toRational)
instance QueryRunnerColumnDefault SqlFloat8 NominalDiffTime where
  defaultFromField = fieldQueryRunnerColumn
instance FF.FromField NominalDiffTime where
  fromField = fmap (fromRational . toRational) <<$>> (FF.fromField :: FF.FieldParser Double)
