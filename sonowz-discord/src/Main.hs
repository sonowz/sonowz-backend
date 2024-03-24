module Main where

import Discord
import Discord.Internal.Rest (Channel (ChannelDirectMessage), DiscordId (DiscordId), User (userId), UserId)
import Discord.Requests
import Polysemy.Embed (runEmbedded)
import Sonowz.Core.Error.Effect (unsafeErrorToIO)
import Sonowz.Discord.Imports
import Sonowz.Discord.REST.Effect (DiscordREST, restCall', runDiscordRESTIO)
import Sonowz.Discord.Utils (getOrCreateChannel)

sonowzId :: UserId
sonowzId = DiscordId 0

testHandler :: DiscordHandler ()
testHandler =
  action
    & runDiscordRESTIO
    & unsafeErrorToIO
    & runEmbedded @IO liftIO
    & runM
  where
    action :: Members [Embed DiscordHandler, DiscordREST] r => Sem r ()
    action = do
      chan <- getOrCreateChannel sonowzId
      restCall' (CreateMessage chan "hello world!")
      embed @DiscordHandler $ liftIO $ print "done"
      pass

isDM :: Channel -> Bool
isDM (ChannelDirectMessage _ users _) = any isUser users
  where
    isUser user = userId user == sonowzId
isDM _ = False

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- For debugging
  hSetBuffering stderr LineBuffering

  error <-
    runDiscord $
      def
        { discordToken = "",
          discordOnStart = testHandler
        }
  print error
