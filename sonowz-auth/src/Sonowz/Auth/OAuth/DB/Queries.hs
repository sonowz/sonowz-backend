{-# LANGUAGE Arrows #-}

module Sonowz.Auth.OAuth.DB.Queries
  ( selectOrInsertOAuthUser,
    selectUser,
    selectTotalUserCount,
  )
where

import Control.Arrow
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Transaction (withTransaction)
import Opaleye
import Sonowz.Auth.Imports hiding (null)
import Sonowz.Auth.OAuth (OAuthUser (..))
import Sonowz.Auth.OAuth.DB.Types
import Sonowz.Core.DB.CRUD
import Sonowz.Core.DB.Field (Uid)
import Sonowz.Core.DB.Utils (maybeToExceptionIO, nullify)

-- Table declarations --

{-
CREATE TABLE public.user_info (
    uid serial PRIMARY KEY NOT NULL,
    oauth_provider text NOT NULL,
    oauth_id text NOT NULL,
    representation text,
    created_time timestamp with time zone DEFAULT now() NOT NULL
);
ALTER TABLE ONLY public.user_info
    ADD CONSTRAINT unique_oauth UNIQUE (oauth_provider, oauth_id);
-}

userTable :: UserTable
userTable = table "user_info" (pUser userFields)

userFields =
  User
    { uid = tableField "uid",
      oauthProvider = tableField "oauth_provider",
      oauthId = tableField "oauth_id",
      representation = tableField "representation",
      createdTime = tableField "created_time"
    }

emptyAggUser =
  User
    { uid = nullify,
      oauthProvider = nullify,
      oauthId = nullify,
      representation = nullify,
      createdTime = nullify
    }

-- Public Interfaces --

selectOrInsertOAuthUser :: HasCallStack => Connection -> OAuthUser -> IO UserInfo
selectOrInsertOAuthUser conn (oauthToHaskW -> writeFields) = withTransaction conn $ do
  maybeUser <- (<|>) <$> selectResult <*> insertResult
  maybeToExceptionIO "Insert/Select userInfo failed" maybeUser
  where
    selectResult = listToMaybe <$> runSelect conn (qSelectUserByOAuth userTable writeFields)
    insertResult = listToMaybe <$> runInsert_ conn (qInsertUser userTable writeFields)

selectUser :: HasCallStack => Connection -> Uid -> IO (Maybe UserInfo)
selectUser = crudRead crudSet

selectTotalUserCount :: HasCallStack => Connection -> IO Int
selectTotalUserCount conn = toInt <<$>> headOrError =<< runSelect conn (qSelectUserCount userTable)
  where
    headOrError = maybeToExceptionIO "Counting failed" . viaNonEmpty head
    toInt = fromIntegral :: Int64 -> Int

-- Queries --

qSelectUserByOAuth :: UserTable -> UserInfoWriteDto -> Select UserFieldR
qSelectUserByOAuth table user = proc () -> do
  selected <- selectTable table -< ()
  restrict -< toFields (oauthProvider user) .== oauthProvider selected
  restrict -< toFields (oauthId user) .== oauthId selected
  returnA -< selected

qSelectUserCount :: UserTable -> Select (Field SqlInt8)
qSelectUserCount table =
  uid <$> aggregate (pUser $ emptyAggUser {uid = count}) (selectTable table)

qInsertUser :: UserTable -> UserInfoWriteDto -> Insert [UserInfo]
qInsertUser table user =
  Insert
    { iTable = table,
      iRows = [toFields user],
      iReturning = rReturning id,
      iOnConflict = Just DoNothing
    }

-- Private Functions --

crudSet :: CRUDQueries UserInfo UserInfoWriteDto Uid
crudSet = getCRUDQueries userTable uid

oauthToHaskW :: OAuthUser -> UserInfoWriteDto
oauthToHaskW OAuthUser {..} =
  User
    { uid = Nothing,
      oauthProvider = oauthUserProvider,
      oauthId = oauthUserId,
      representation = oauthUserRep,
      createdTime = Nothing
    }
