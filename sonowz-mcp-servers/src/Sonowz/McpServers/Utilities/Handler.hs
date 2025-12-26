module Sonowz.McpServers.Utilities.Handler
  ( Tools (..),
    handlers,
    descriptions,
  )
where

import MCP.Server (Content (ContentText))
import Sonowz.McpServers.Imports

data Tools = GetCurrentTime | Wait {seconds :: Int}

descriptions :: [(String, String)]
descriptions =
  [ ("GetCurrentTime", "Returns current datetime."),
    ("Wait", "Wait for the specified number of seconds before responding."),
    ("seconds", "Number of seconds to wait.")
  ]

handlers :: Tools -> IO Content
handlers (GetCurrentTime) = return $ ContentText "test"
handlers (Wait secs) = undefined