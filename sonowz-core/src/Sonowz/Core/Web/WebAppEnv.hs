module Sonowz.Core.Web.WebAppEnv
  ( WebAppEnv(..)
  )
where

import URI.ByteString (URI)

import Sonowz.Core.Imports

data WebAppEnv = WebAppEnv
  { eWebDomain :: URI
  , eWebAPIRoot :: Text -- example: "/api"
  }
