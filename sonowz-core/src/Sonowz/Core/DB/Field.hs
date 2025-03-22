module Sonowz.Core.DB.Field
  ( Uid (..),
    EmptyField,
  )
where

import Data.Aeson (FromJSON (parseJSON), ToJSON)
import Data.Profunctor.Product.Default (Default (def))
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Opaleye (DefaultFromField, Field, Field_, SqlInt4, ToFields, toToFields)
import Servant (FromHttpApiData, parseQueryParam)
import Sonowz.Core.Imports

newtype Uid = Uid Int
  deriving (Eq, Show, Read)
  deriving newtype (ToField, FromField)
  deriving (Num, ToJSON, FromJSON) via Int

deriving via Int instance DefaultFromField SqlInt4 Uid

instance Default ToFields Uid (Field SqlInt4) where
  def = coerce (def :: ToFields Int (Field SqlInt4))

instance FromHttpApiData Uid where
  parseQueryParam = fmap Uid . readEither @Int . toString

-- Use this type to indicate that this field is not used in writes
type EmptyField = Maybe EmptyField_

data EmptyField_

instance Default ToFields EmptyField_ (Field_ n col) where
  def = toToFields (error "Unexpected 'EmptyField' access")

instance FromJSON EmptyField_ where
  parseJSON = error "Unexpected 'EmptyField' access"
