{-# LANGUAGE Arrows #-}
module Sonowz.Web.KVS.DB.Queries
  ( getKeyValue
  , setKeyValue
  , deleteKey
  ) where

import Control.Arrow
import qualified Control.Exception.Safe as E
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Transaction (withTransaction)
import Opaleye
import qualified Relude.Unsafe as Unsafe
import Sonowz.Core.DB.Utils (DatabaseException(..), maybeToExceptionIO)
import Sonowz.Web.Imports
import Sonowz.Web.KVS.DB.Types


-- Table declarations --

{-
CREATE TABLE public.web_kvs (
    uid serial PRIMARY KEY NOT NULL,
    oauth_id text NOT NULL,
    key text NOT NULL,
    value text NOT NULL,
    created_time timestamp with time zone DEFAULT now() NOT NULL,
    updated_time timestamp with time zone DEFAULT now() NOT NULL
);
CREATE TRIGGER web_kvs_update BEFORE UPDATE ON public.web_kvs FOR EACH ROW EXECUTE PROCEDURE public.update_time();
-}

kvsTable :: KVSTable
kvsTable = table "web_kvs" (pKVS kvsFields)

kvsFields = KVS
  { uid         = tableField "uid"
  , oauthId     = tableField "oauth_id"
  , key         = tableField "key"
  , value       = tableField "value"
  , createdTime = tableField "created_time"
  , updatedTime = tableField "updated_time"
  }

-- Public Interfaces --

getKeyValue :: HasCallStack => Connection -> Text -> Text -> IO (Maybe Text)
getKeyValue conn oauthId key = withTransaction conn (value <<$>> selectResult)
 where
  selectResult :: IO (Maybe KVSHaskR)
  selectResult = listToMaybe <$> runSelect conn (qSelectKeyValue kvsTable oauthId key)

setKeyValue :: HasCallStack => Connection -> Text -> Text -> Text -> IO ()
setKeyValue conn oauthId key value = withTransaction conn $ do
  selectResult >>= \case
    0 -> void $ maybeToExceptionIO "'setKeyValue' failed: insert" =<< insertResult
    1 -> void $ maybeToExceptionIO "'setKeyValue' failed: update" =<< updateResult
    _ -> E.throw $ DatabaseException "'setKeyValue' failed: multiple instance for key"
 where
  selectResult :: IO Int64
  selectResult =
    Unsafe.head <<$>> runSelect conn $ countRows (qSelectKeyValue kvsTable oauthId key)
  insertResult = listToMaybe <$> runInsert_ conn (qInsertKeyValue kvsTable writeFields)
  updateResult = listToMaybe <$> runUpdate_ conn (qUpdateKeyValue kvsTable oauthId key value)
  writeFields  = makeHaskW oauthId key value

deleteKey :: HasCallStack => Connection -> Text -> Text -> IO ()
deleteKey conn oauthId key =
  withTransaction conn
    $   (\n -> if n /= 1 then E.throw $ DatabaseException "'deleteKey' failed" else pass)
    =<< deleteResult
  where deleteResult = runDelete_ conn (qDeleteKeyValue kvsTable oauthId key)


-- Private Functions --

makeHaskW :: Text -> Text -> Text -> KVSHaskW
makeHaskW oauthId key value = KVS
  { uid         = error "Unexpected 'uid' access"
  , oauthId     = oauthId
  , key         = key
  , value       = value
  , createdTime = error "Unexpected 'createdTime' access"
  , updatedTime = error "Unexpected 'updatedTime' access"
  }


-- Queries --

qSelectKeyValue :: KVSTable -> Text -> Text -> Select KVSR
qSelectKeyValue table _oauthId _key = proc () -> do
  selected <- selectTable table -< ()
  restrict -< toFields _oauthId .== oauthId selected
  restrict -< toFields _key .== key selected
  returnA -< selected

qInsertKeyValue :: KVSTable -> KVSHaskW -> Insert [KVSHaskR]
qInsertKeyValue table kv = Insert
  { iTable      = table
  , iRows       = [toFields kv]
  , iReturning  = rReturning id
  , iOnConflict = Just DoNothing
  }

qUpdateKeyValue :: KVSTable -> Text -> Text -> Text -> Update [KVSHaskR]
qUpdateKeyValue table _oauthId _key _value = Update
  { uTable      = table
  , uUpdateWith = updateEasy (\row -> row { value = toFields _value })
  , uWhere      = \row -> toFields _oauthId .== oauthId row .&& toFields _key .== key row
  , uReturning  = rReturning id
  }

qDeleteKeyValue :: KVSTable -> Text -> Text -> Delete Int64
qDeleteKeyValue table _oauthId _key = Delete
  { dTable     = table
  , dWhere     = \row -> toFields _oauthId .== oauthId row .&& toFields _key .== key row
  , dReturning = rCount
  }
