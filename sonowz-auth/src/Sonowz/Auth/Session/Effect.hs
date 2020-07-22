{-# LANGUAGE TemplateHaskell #-}
module Sonowz.Auth.Session.Effect
  ( Session
  , SessionKey
  , ExpirySec
  , getSession
  , setSession
  , runSessionToIO
  )
where

import Data.Fixed (Fixed(MkFixed))
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.LocalTime (LocalTime, zonedTimeToLocalTime, addLocalTime)
import Data.HashMap.Strict as H

import Sonowz.Auth.Imports
import Sonowz.Core.Time.Effect (Time, getTime, timeToIO)

type SessionKey = Text

type ExpirySec = Int
type SessionStore v = H.HashMap SessionKey (LocalTime, v)
type SessionRef v = IORef (SessionStore v)

data Session (v :: *) m a where
  GetSession :: SessionKey -> Session v m (Maybe v)
  SetSession :: SessionKey -> ExpirySec -> v -> Session v m ()

makeSem ''Session


runSessionToIO :: Member (Embed IO) r => Sem (Session v : r) a -> Sem r a
runSessionToIO action = do
  storeRef <- liftIO (newIORef mempty)
  timeToIO . runReader storeRef . runSessionAsReaderIORef . raiseUnder2 $ action

type ReaderIORefEffects v = '[Embed IO, Time, Reader (SessionRef v)]
runSessionAsReaderIORef
  :: forall v r a
   . Members (ReaderIORefEffects v) r
  => Sem (Session v : r) a
  -> Sem r a
runSessionAsReaderIORef = interpret $ \case
  GetSession k   -> maybe (return Nothing) checkValid =<< (H.lookup k <$> askStore)
  SetSession k s v -> void $ H.insert k <$> withExpiry s v <*> askStore
 where
  askStore :: Members (ReaderIORefEffects v) r => Sem r (SessionStore v)
  askStore = liftIO . readIORef =<< ask
  checkValid :: Members (ReaderIORefEffects v) r => (LocalTime, v) -> Sem r (Maybe v)
  checkValid (expiry, v) = do
    time <- zonedTimeToLocalTime <$> getTime
    return $ if expiry > time then Nothing else Just v
  withExpiry :: Members (ReaderIORefEffects v) r => ExpirySec -> v -> Sem r (LocalTime, v)
  withExpiry sec v = do
    time <- zonedTimeToLocalTime <$> getTime
    let expiryTime = secondsToNominalDiffTime (toFixed sec) `addLocalTime` time
    return (expiryTime, v)
  toFixed = MkFixed . fromIntegral

  
