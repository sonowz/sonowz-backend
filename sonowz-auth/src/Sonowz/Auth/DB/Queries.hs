{-# LANGUAGE Arrows #-}
module Sonowz.Auth.DB.Queries where

import Control.Arrow
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Transaction (withTransaction)
import Opaleye


import Sonowz.Auth.Imports hiding (null)

import Sonowz.Auth.DB.Types
import Sonowz.Auth.OAuth (OAuthUser(..))
import Sonowz.Core.DB.Utils (maybeToExceptionIO, nullify)


-- Table declarations --

userTable :: UserTable
userTable = table "user_info" (pUser userFields)

userFields = User
  { uid            = tableField "uid"
  , oauthProvider  = tableField "oauth_provider"
  , oauthId        = tableField "oauth_id"
  , representation = tableField "representation"
  , createdTime    = tableField "created_time"
  }

emptyAggUser = User
  { uid            = nullify
  , oauthProvider  = nullify
  , oauthId        = nullify
  , representation = nullify
  , createdTime    = nullify
  }

-- Public Interfaces --

insertOAuthUser :: HasCallStack => Connection -> OAuthUser -> IO UserInfo
insertOAuthUser conn (oauthToWriteField -> writeFields) = withTransaction conn $ do
  maybeUser <- (<|>) <$> selectResult <*> insertResult
  maybeToExceptionIO "Insert/Select userInfo failed" maybeUser
 where
  selectResult = listToMaybe <$> runSelect conn (qSelectUserByOAuth userTable writeFields)
  insertResult = listToMaybe <$> runInsert_ conn (qInsertUser userTable writeFields)

selectUser :: HasCallStack => Connection -> Uid -> IO (Maybe UserInfo)
selectUser conn uid = listToMaybe <$> runSelect conn (qSelectUserByUid userTable uid)

selectTotalUserCount :: HasCallStack => Connection -> IO Int
selectTotalUserCount conn = toInt <<$>> headOrError =<< runSelect conn (qSelectUserCount userTable) where
  headOrError = maybeToExceptionIO "Counting failed" . viaNonEmpty head
  toInt       = fromIntegral :: Int64 -> Int


-- Queries --

qSelectUserByOAuth :: UserTable -> UserFieldW -> Select UserFieldR
qSelectUserByOAuth table user = proc () -> do
  selected <- selectTable table -< ()
  restrict -< oauthProvider user .== oauthProvider selected
  restrict -< oauthId user .== oauthId selected
  returnA  -< selected

qSelectUserByUid :: UserTable -> Uid -> Select UserFieldR
qSelectUserByUid table _uid = proc () -> do
  selected <- selectTable table -< ()
  restrict -<  uid selected .== toFields _uid
  returnA  -< selected

qSelectUserCount :: UserTable -> Select (Field SqlInt8)
qSelectUserCount table = uid
  <$> aggregate (pUser $ emptyAggUser { uid = count }) (selectTable table) where

qInsertUser :: UserTable -> UserFieldW -> Insert [UserInfo]
qInsertUser table user = Insert
  { iTable      = table
  , iRows       = [user]
  , iReturning  = rReturning id
  , iOnConflict = Just DoNothing
  }


-- Private Functions --

oauthToWriteField :: OAuthUser -> UserFieldW
oauthToWriteField OAuthUser {..} = User
  { uid            = Nothing
  , oauthProvider  = toFields oauthUserProvider
  , oauthId        = toFields oauthUserId
  , representation = toFields oauthUserRep
  , createdTime    = Nothing
  }

insertIsSuccess :: Int64 -> Bool
insertIsSuccess result = result > 0
