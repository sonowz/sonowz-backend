{-# LANGUAGE TemplateHaskell #-}

module Sonowz.Discord.REST.Effect
  ( DiscordREST,
    restCall',
    runDiscordRESTIO,
  )
where

import Data.Aeson (FromJSON)
import Discord (DiscordHandler, RestCallErrorCode, restCall)
import Discord.Internal.Rest (Request)
import Sonowz.Discord.Imports

data DiscordREST m a where
  RestCall' :: (FromJSON a, Request (r a)) => r a -> DiscordREST m a

makeSem ''DiscordREST

runDiscordRESTIO ::
  Members '[Embed DiscordHandler, Error RestCallErrorCode] r =>
  Sem (DiscordREST : r) a ->
  Sem r a
runDiscordRESTIO = interpret $ \case
  RestCall' req -> fromEither =<< embed (restCall req)
