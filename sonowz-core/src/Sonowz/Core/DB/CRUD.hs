{-# LANGUAGE Arrows #-}
module Sonowz.Core.DB.CRUD
  ( create
  , read
  , update
  , delete
  , list
  , getCRUDQueries
  , CRUDQueries(crudCreate, crudDelete, crudList, crudRead, crudUpdate)
  ) where

import Control.Arrow
import Data.Profunctor.Product.Default (Default)
import Database.PostgreSQL.Simple (Connection, withTransaction)
import Opaleye
import Sonowz.Core.Imports


data CRUDQueries item citem uid = CRUDQueries
  { crudList   :: Connection -> IO [item]
  , crudRead   :: Connection -> uid -> IO (Maybe item)
  , crudCreate :: Connection -> citem -> IO (Maybe item)
  , crudUpdate :: Connection -> uid -> citem -> IO (Maybe item)
  , crudDelete :: Connection -> uid -> IO Bool
  }

-- Utility function for generating all CRUD queries
getCRUDQueries
  :: ( Default FromFields r item
     , Default ToFields citem w
     , Default Unpackspec r r
     , Default ToFields uid (Column uidcol)
     )
  => Table w r
  -> (r -> Column uidcol)
  -> CRUDQueries item citem uid
getCRUDQueries table rowToUid = CRUDQueries
  (list table)
  (read table rowToUid)
  (create table)
  (update table rowToUid)
  (delete table rowToUid)


qSelectByUid
  :: (Default Unpackspec r r, Default ToFields uid (Column uidcol))
  => Table w r
  -> (r -> Column uidcol)
  -> uid
  -> Select r
qSelectByUid table rowToUid uid = proc () -> do
  selected <- selectTable table -< ()
  restrict -< rowToUid selected .== toFields uid
  returnA -< selected


list
  :: (HasCallStack, Default FromFields r hask, Default Unpackspec r r)
  => Table w r
  -> Connection
  -> IO [hask]
list table conn = withTransaction conn $ runSelect conn (selectTable table)


read
  :: ( HasCallStack
     , Default FromFields r hask
     , Default Unpackspec r r
     , Default ToFields uid (Column uidcol)
     )
  => Table w r
  -> (r -> Column uidcol)
  -> Connection
  -> uid
  -> IO (Maybe hask)
read table rowToUid conn uid = withTransaction conn $ oneListToMaybe <$> runSelect conn query
  where query = qSelectByUid table rowToUid uid


create
  :: (HasCallStack, Default FromFields r hask, Default ToFields chask w, Default Unpackspec r r)
  => Table w r
  -> Connection
  -> chask
  -> IO (Maybe hask)
create table conn item = withTransaction conn $ oneListToMaybe <$> runInsert_ conn query where
  query = Insert
    { iTable      = table
    , iRows       = [toFields item]
    , iReturning  = rReturning id
    , iOnConflict = Nothing -- Raises exception when conflict
    }


update
  :: ( HasCallStack
     , Default FromFields r hask
     , Default ToFields chask w
     , Default Unpackspec r r
     , Default ToFields uid (Column uidcol)
     )
  => Table w r
  -> (r -> Column uidcol)
  -> Connection
  -> uid
  -> chask
  -> IO (Maybe hask)
update table rowToUid conn uid item = withTransaction conn $ do
  (targetCount :: Int64) <- unsafeHead <<$>> runSelect conn $ countRows $ qSelectByUid
    table
    rowToUid
    uid
  guard (targetCount == 1) -- Raises exception when update target is not one row
  oneListToMaybe <$> runUpdate_ conn query where
  query = Update
    { uTable      = table
    , uUpdateWith = const (toFields item)
    , uWhere      = \row -> rowToUid row .== toFields uid
    , uReturning  = rReturning id
    }


delete
  :: (HasCallStack, Default Unpackspec r r, Default ToFields uid (Column uidcol))
  => Table w r
  -> (r -> Column uidcol)
  -> Connection
  -> uid
  -> IO Bool
delete table rowToUid conn uid = withTransaction conn $ do
  (targetCount :: Int64) <- unsafeHead <<$>> runSelect conn $ countRows $ qSelectByUid
    table
    rowToUid
    uid
  guard (targetCount == 1) -- Raises exception when delete target is not one row
  (== 1) <$> runDelete_ conn query where
  query =
    Delete { dTable = table, dWhere = \row -> rowToUid row .== toFields uid, dReturning = rCount }


oneListToMaybe :: [a] -> Maybe a
oneListToMaybe [x] = Just x
oneListToMaybe _   = Nothing

unsafeHead :: [a] -> a
unsafeHead = fromMaybe (error "List is empty!") . viaNonEmpty head
