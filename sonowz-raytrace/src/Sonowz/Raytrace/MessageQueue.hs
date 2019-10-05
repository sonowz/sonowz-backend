module Sonowz.Raytrace.MessageQueue where

import Relude

type DBConnection = ()
_init = undefined
enqueue = undefined
dequeue = undefined
popFrontMessage = undefined
getStatus :: DBConnection -> IO [(Int, Int)]
getStatus = undefined
{--
module Sonowz.Raytrace.MessageQueue
  ( DBConnection
  , _init
  , popFrontMessage
  , enqueue
  , dequeue
  , getStatus
  )
where

import           Relude
import           Control.Monad
import           Control.Exception
-- import Database.PostgreSQL.Simple


type DBConnection = Connection

_init :: IO Connection
_init = do
  conn <- connectDB
  execute_ conn createTable
  execute_ conn clearTable
  return conn

popFrontMessage :: Connection -> IO (Maybe (Int, String))
popFrontMessage conn = do
  result <- (query_ conn getFront :: IO [(Int, String)])
  case result of
    []             -> return Nothing
    [(id, config)] -> do
      execute_ conn popFront
      return $ Just (id, config)

-- TODO return remainingQueue
enqueue :: Connection -> Int -> String -> IO (Maybe Int64)
enqueue conn id config = do
  result <- catch (liftM Just $ execute conn enqueueMessage (id, config))
                  errorHandler
  return result

dequeue :: Connection -> Int -> IO (Maybe Bool)
dequeue conn id = do
  result <-
    catch (liftM Just $ query conn dequeueMessage [id]) errorHandler :: IO
      (Maybe [Only Int])
  case result of
    Nothing       -> return Nothing
    Just []       -> return $ Just False
    Just [Only _] -> return $ Just True

getStatus :: Connection -> IO [(Int, Int)]
getStatus conn = do
  result <- (query_ conn getOrderedIDs :: IO [(Only Int)])
  case result of
    [] -> return []
    -- Only id -> (id, order)
    _  -> return $ zip (map (\(Only id) -> id) result) [1 ..]


-- TODO make config file
-- The port is secured, and 'raytrace_mq' user has limited access
connectDB :: IO Connection
connectDB =
  connectPostgreSQL "host=localhost port=5432 user=raytrace_mq password=bestrt"

errorHandler :: SqlError -> IO (Maybe a)
errorHandler e = do
  putStrLn ("DBError: " ++ show (sqlErrorMsg e))
  return Nothing

---- Queries ----

createTable =
  " \
  \CREATE TABLE IF NOT EXISTS raytrace_mq (\
    \id int not null, \
    \config text not null, \
    \createdTime timestamp default current_timestamp PRIMARY KEY \
  \);"

clearTable = "DELETE FROM raytrace_mq"

getFront =
  " \
  \SELECT id, config FROM raytrace_mq \
  \WHERE createdTime = (SELECT min(createdTime) FROM raytrace_mq);"

popFront =
  " \
  \DELETE FROM raytrace_mq \
  \WHERE createdTime = (SELECT min(createdTime) FROM raytrace_mq);"

enqueueMessage = " \
  \INSERT INTO raytrace_mq \
  \VALUES (?, ?);"

dequeueMessage =
  " \
  \DELETE FROM raytrace_mq \
  \WHERE id = ? \
  \RETURNING id;"

getOrderedIDs =
  " \
  \SELECT id FROM raytrace_mq \
  \ORDER BY createdTime ASC;"
--}