module Main where

import Discord
import Discord.Internal.Rest (Channel (ChannelDirectMessage, channelId), Snowflake (Snowflake), User (userId), UserId)
import Discord.Requests
import GHC.IO.Handle (BufferMode (LineBuffering), hSetBuffering)
import Sonowz.Discord.Imports
import Sonowz.Discord.REST.Effect (DiscordREST, restCall', runDiscordRESTIO)
import Sonowz.Discord.Utils (getOrCreateChannel)

sonowzId :: UserId
sonowzId = Snowflake 0

testHandler :: DiscordHandler ()
testHandler =
  action
    & runDiscordRESTIO
    & runError
    & fmap (either (error . show) id)
    & runM
  where
    action :: Members [Embed DiscordHandler, DiscordREST] r => Sem r ()
    action = do
      chan <- getOrCreateChannel sonowzId
      restCall' (CreateMessage chan "hello world!")
      embed @DiscordHandler $ unsafeLiftIO $ print "done"
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
