{-# LANGUAGE Arrows #-}
module Sonowz.Auth.DB.Queries where

import Opaleye
import Control.Arrow
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Transaction (withTransaction)
import Data.Profunctor (dimap)
import qualified Control.Exception.Safe as E
import qualified Opaleye.Aggregate as Agg

import Sonowz.Auth.Imports hiding (null)

import Sonowz.Auth.DB.Types
import Sonowz.Auth.OAuth (OAuthUser(..))


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
  , createdTime    = nullify }

-- Public Interfaces --

insertOAuthUser :: HasCallStack => Connection -> OAuthUser -> IO UserInfo
insertOAuthUser conn (oauthToWriteField -> writeFields) = withTransaction conn $ do
  maybeUser <- (<|>) <$> selectResult <*> insertResult
  justOrThrow "Insert/Select userInfo failed" maybeUser
  where
    selectResult = listToMaybe <$> runSelect conn (selectUserByOAuth userTable writeFields)
    insertResult = listToMaybe <$> runInsert_ conn (insertUser userTable writeFields)

selectUser :: HasCallStack => Connection -> Uid -> IO (Maybe UserInfo)
selectUser conn uid = listToMaybe <$> runSelect conn (selectUserByUid userTable uid)

selectTotalUserCount :: HasCallStack => Connection -> IO Int
selectTotalUserCount conn = toInt <<$>> headOrError =<< runSelect conn (selectUserCount userTable) where
  headOrError = justOrThrow "Counting failed" . viaNonEmpty head
  toInt       = fromIntegral :: Int64 -> Int

-- Private Functions --

oauthToWriteField :: OAuthUser -> UserFieldW
oauthToWriteField OAuthUser{..} = User
  { uid            = Nothing
  , oauthProvider  = toFields oauthUserProvider
  , oauthId        = toFields oauthUserId
  , representation = toFields oauthUserRep
  , createdTime    = Nothing
  }

-- Queries --

selectUserByOAuth :: UserTable -> UserFieldW -> Select UserFieldR
selectUserByOAuth table user = proc () -> do
  selected <- selectTable table -< ()
  restrict -< oauthProvider user .== oauthProvider selected
  restrict -< oauthId user .== oauthId selected
  returnA  -< selected

selectUserByUid :: UserTable -> Uid -> Select UserFieldR
selectUserByUid table _uid = proc () -> do
  selected <- selectTable table -< ()
  restrict -<  uid selected .== toFields _uid
  returnA  -< selected

selectUserCount :: UserTable -> Select (Field SqlInt8)
selectUserCount table = uid <$> aggregate (pUser $ emptyAggUser { uid = count }) (selectTable table) where

insertUser :: UserTable -> UserFieldW -> Insert [UserInfo]
insertUser table user = Insert
  { iTable      = table
  , iRows       = [user]
  , iReturning  = rReturning id
  , iOnConflict = Just DoNothing
  } 


-- Opaleye helper functions --

nullify :: Aggregator (Field a) (FieldNullable a)
nullify = dimap id (const null) Agg.count

putArg :: SelectArr a b -> a -> Select b
putArg f a = arr (const a) >>> f

insertIsSuccess :: Int64 -> Bool
insertIsSuccess result = result > 0

logWarnDB :: HasCallStack => IO ()
logWarnDB = withFrozenCallStack $ logWarningIO "Query result might be wrong!"

logCheckBool :: HasCallStack => Bool -> IO Bool
logCheckBool False = withFrozenCallStack $ logWarnDB >> return False
logCheckBool True = withFrozenCallStack $ return True

logCheckMaybe :: HasCallStack => Maybe a -> IO (Maybe a)
logCheckMaybe Nothing = withFrozenCallStack $ logWarnDB >> return Nothing
logCheckMaybe x = withFrozenCallStack $ return x

justOrThrow :: HasCallStack => Text -> Maybe a -> IO a
justOrThrow msg = withFrozenCallStack $ maybe (E.throw $ DatabaseException msg) return