{-# LANGUAGE TemplateHaskell #-}

module Sonowz.McpServers.Utilities.Server
  ( utilitiesHandlers,
  )
where

import MCP.Server (McpServerHandlers (..))
import MCP.Server.Derive (deriveToolHandlerWithDescription)
import Sonowz.McpServers.Imports
import Sonowz.McpServers.Utilities.Handler (Tools (..), descriptions, handlers)

utilitiesHandlers :: McpServerHandlers IO
utilitiesHandlers =
  McpServerHandlers
    { prompts = Nothing,
      resources = Nothing,
      tools = Just $(deriveToolHandlerWithDescription ''Tools 'handlers descriptions)
    }