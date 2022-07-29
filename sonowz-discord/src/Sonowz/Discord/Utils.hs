module Sonowz.Discord.Utils
  ( getOrCreateChannel,
  )
where

import Discord.Requests (UserRequest (CreateDM, GetUserDMs))
import Discord.Types (Channel (ChannelDirectMessage, channelId), ChannelId, User (userId), UserId)
import Sonowz.Discord.Imports
import Sonowz.Discord.REST.Effect (DiscordREST, restCall')

getOrCreateChannel :: Member DiscordREST r => UserId -> Sem r ChannelId
getOrCreateChannel targetUserId = do
  dmChannels <- restCall' GetUserDMs
  dmChannel <- case find isUserChannel dmChannels of
    Just existingChannel -> return existingChannel
    Nothing -> restCall' (CreateDM targetUserId)
  return (channelId dmChannel)
  where
    isUserChannel :: Channel -> Bool
    isUserChannel (ChannelDirectMessage _ userList _) =
      any (\u -> userId u == targetUserId) userList
    isUserChannel _ = False
