{-# LANGUAGE Arrows #-}

module Sonowz.Core.DB.CRUD
  ( create,
    read,
    update,
    delete,
    list,
    getCRUDQueries,
    CRUDQueries (..),
  )
where

import Control.Arrow
import Data.Profunctor (Profunctor (lmap, rmap))
import Data.Profunctor.Product.Default (Default)
import Database.PostgreSQL.Simple (Connection, withTransaction)
import Opaleye
import Sonowz.Core.DB.Utils (AlternativeUpdater, updateAlternative)
import Sonowz.Core.Imports

data CRUDQueries uid wdto dto = CRUDQueries
  { crudList :: Connection -> IO [dto],
    crudRead :: Connection -> uid -> IO (Maybe dto),
    crudCreate :: Connection -> wdto -> IO (Maybe dto),
    crudUpdate :: Connection -> uid -> wdto -> IO (Maybe dto),
    crudDelete :: Connection -> uid -> IO Bool
  }

-- Use Profunctor to compose 'mapper' with CRUDQueries
instance Profunctor (CRUDQueries uid) where
  lmap :: (model -> wdto) -> CRUDQueries uid wdto dto -> CRUDQueries uid model dto
  lmap f CRUDQueries {..} =
    CRUDQueries
      { crudList = crudList,
        crudRead = crudRead,
        crudCreate = \conn wdto -> crudCreate conn (f wdto),
        crudUpdate = \conn uid wdto -> crudUpdate conn uid (f wdto),
        crudDelete = crudDelete
      }
  rmap :: (dto -> model) -> CRUDQueries uid wdto dto -> CRUDQueries uid wdto model
  rmap f CRUDQueries {..} =
    CRUDQueries
      { crudList = \conn -> f <<$>> crudList conn,
        crudRead = \conn uid -> f <<$>> crudRead conn uid,
        crudCreate = \conn wdto -> f <<$>> crudCreate conn wdto,
        crudUpdate = \conn uid wdto -> f <<$>> crudUpdate conn uid wdto,
        crudDelete = crudDelete
      }

-- Utility function for generating all CRUD queries
getCRUDQueries ::
  ( Default FromFields r dto,
    Default ToFields wdto w,
    Default Unpackspec r r,
    Default Updater r w,
    Default AlternativeUpdater w w,
    Default ToFields uid (Field uidcol)
  ) =>
  Table w r ->
  (r -> Field uidcol) ->
  CRUDQueries uid wdto dto
getCRUDQueries table rowToUid =
  CRUDQueries
    (list table)
    (read table rowToUid)
    (create table)
    (update table rowToUid)
    (delete table rowToUid)

qSelectByUid ::
  (Default Unpackspec r r, Default ToFields uid (Field uidcol)) =>
  Table w r ->
  (r -> Field uidcol) ->
  uid ->
  Select r
qSelectByUid table rowToUid uid = proc () -> do
  selected <- selectTable table -< ()
  restrict -< rowToUid selected .== toFields uid
  returnA -< selected

list ::
  (HasCallStack, Default FromFields r dto, Default Unpackspec r r) =>
  Table w r ->
  Connection ->
  IO [dto]
list table conn = withTransaction conn $ runSelect conn (selectTable table)

read ::
  ( HasCallStack,
    Default FromFields r dto,
    Default Unpackspec r r,
    Default ToFields uid (Field uidcol)
  ) =>
  Table w r ->
  (r -> Field uidcol) ->
  Connection ->
  uid ->
  IO (Maybe dto)
read table rowToUid conn uid = withTransaction conn $ oneListToMaybe <$> runSelect conn query
  where
    query = qSelectByUid table rowToUid uid

create ::
  (HasCallStack, Default FromFields r dto, Default ToFields wdto w, Default Unpackspec r r) =>
  Table w r ->
  Connection ->
  wdto ->
  IO (Maybe dto)
create table conn item = withTransaction conn $ oneListToMaybe <$> runInsert_ conn query
  where
    query =
      Insert
        { iTable = table,
          iRows = [toFields item],
          iReturning = rReturning id,
          iOnConflict = Nothing -- Raises exception when conflict
        }

update ::
  ( HasCallStack,
    Default FromFields r dto,
    Default ToFields wdto w,
    Default Unpackspec r r,
    Default Updater r w,
    Default AlternativeUpdater w w,
    Default ToFields uid (Field uidcol)
  ) =>
  Table w r ->
  (r -> Field uidcol) ->
  Connection ->
  uid ->
  wdto ->
  IO (Maybe dto)
update table rowToUid conn uid item = withTransaction conn $ do
  (targetCount :: Int64) <-
    unsafeHead <<$>> runSelect conn $ countRows $ qSelectByUid table rowToUid uid
  guard (targetCount == 1) -- Raises exception when update target is not one row
  oneListToMaybe <$> runUpdate_ conn query
  where
    query =
      Update
        { uTable = table,
          uUpdateWith = updateAlternative (toFields item),
          uWhere = \row -> rowToUid row .== toFields uid,
          uReturning = rReturning id
        }

delete ::
  (HasCallStack, Default Unpackspec r r, Default ToFields uid (Field uidcol)) =>
  Table w r ->
  (r -> Field uidcol) ->
  Connection ->
  uid ->
  IO Bool
delete table rowToUid conn uid = withTransaction conn $ do
  (targetCount :: Int64) <-
    unsafeHead <<$>> runSelect conn $ countRows $ qSelectByUid table rowToUid uid
  guard (targetCount == 1) -- Raises exception when delete target is not one row
  (== 1) <$> runDelete_ conn query
  where
    query =
      Delete {dTable = table, dWhere = \row -> rowToUid row .== toFields uid, dReturning = rCount}

oneListToMaybe :: [a] -> Maybe a
oneListToMaybe [x] = Just x
oneListToMaybe _ = Nothing

unsafeHead :: [a] -> a
unsafeHead = fromMaybe (error "List is empty!") . viaNonEmpty head
