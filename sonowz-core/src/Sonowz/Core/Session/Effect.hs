{-# LANGUAGE TemplateHaskell #-}

module Sonowz.Core.Session.Effect
  ( Session,
    SessionKey,
    ExpirySec,
    getSession,
    setSession,
    newSession,
    runSessionToIO,
  )
where

-- This code has no usages currently :( --

import Crypto.Hash (Digest, SHA256, hash)
import Data.Fixed (Fixed (MkFixed))
import Data.HashMap.Strict as H
import Data.Text as T
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.LocalTime (LocalTime, addLocalTime, zonedTimeToLocalTime)
import Sonowz.Core.Imports
import Sonowz.Core.Time.Effect (Time, getTime, timeToIO)
import System.Random (randomIO)

type SessionKey = Text -- Base64-encoded key

type ExpirySec = Int

type SessionStore v = H.HashMap SessionKey (LocalTime, v)

type SessionRef v = IORef (SessionStore v)

data Session (v :: *) m a where
  GetSession :: SessionKey -> Session v m (Maybe v)
  SetSession :: SessionKey -> ExpirySec -> v -> Session v m ()
  NewSession :: ExpirySec -> v -> Session v m SessionKey

makeSem ''Session

runSessionToIO :: (Member (Final IO) r) => Sem (Session v : r) a -> Sem r a
runSessionToIO action = do
  storeRef <- embedFinal (newIORef mempty)
  timeToIO . runReader storeRef . runSessionAsReaderIORef . raiseUnder2 $ action

type ReaderIORefEffects v = '[Final IO, Time, Reader (SessionRef v)]

runSessionAsReaderIORef ::
  forall v r a. (Members (ReaderIORefEffects v) r) => Sem (Session v : r) a -> Sem r a
runSessionAsReaderIORef = interpret $ \case
  GetSession k -> maybe (return Nothing) checkValid . H.lookup k =<< askStore
  SetSession k s v -> void $ H.insert k <$> withExpiry s v <*> askStore
  NewSession s v -> do
    k <- generateKey
    void $ H.insert k <$> withExpiry s v <*> askStore
    return k
  where
    askStore :: (Member (Reader (SessionRef v)) r) => Sem r (SessionStore v)
    askStore = embedFinal . readIORef =<< ask
    checkValid :: (Member Time r) => (LocalTime, v) -> Sem r (Maybe v)
    checkValid (expiry, v) = do
      time <- zonedTimeToLocalTime <$> getTime
      return $ if expiry > time then Nothing else Just v
    withExpiry :: (Member Time r) => ExpirySec -> v -> Sem r (LocalTime, v)
    withExpiry sec v = do
      time <- zonedTimeToLocalTime <$> getTime
      let expiryTime = secondsToNominalDiffTime (toFixed sec) `addLocalTime` time
      return (expiryTime, v)
    -- Generate 64-character key using SHA256
    generateKey :: (Member (Final IO) r) => Sem r SessionKey
    generateKey = T.take 64 . show . sha256 . show <$> generateNumber
      where
        sha256 = hash :: ByteString -> Digest SHA256
        generateNumber = embedFinal randomIO :: (Member (Final IO) r) => Sem r Int64
    toFixed = MkFixed . fromIntegral
