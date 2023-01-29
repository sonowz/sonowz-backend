module Sonowz.Core.DB.Field
  ( Uid (..),
    EmptyField,
  )
where

import Data.Aeson (FromJSON (parseJSON), ToJSON)
import Data.Profunctor.Product.Default (Default (def))
import Opaleye (DefaultFromField, Field, SqlInt4, ToFields, toToFields)
import Servant (FromHttpApiData, parseQueryParam)
import Sonowz.Core.Imports

newtype Uid = Uid Int
  deriving (Eq, Show, Read)
  deriving (Num, ToJSON, FromJSON) via Int

deriving via Int instance DefaultFromField SqlInt4 Uid

instance Default ToFields Uid (Field SqlInt4) where
  def = coerce (def :: ToFields Int (Field SqlInt4))

instance FromHttpApiData Uid where
  parseQueryParam = fmap Uid . readEither @Int . toString
