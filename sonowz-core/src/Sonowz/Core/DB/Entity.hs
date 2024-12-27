module Sonowz.Core.DB.Entity
  ( Entity (..),
    EntityIdField,
  )
where

import Opaleye (SqlInt4)
import Opaleye.Field (Field)
import Sonowz.Core.DB.Field (Uid)
import Sonowz.Core.Imports

class Entity a where
  type EntityId a
  type EntityId a = Uid

  type EntityIdFieldType a
  type EntityIdFieldType a = SqlInt4

  entityIdField :: a -> EntityIdField a

  entityToFields :: Proxy a -> EntityId a -> EntityIdField a

type EntityIdField a = Field (EntityIdFieldType a)