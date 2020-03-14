module Sonowz.Raytrace.Core.Has
  ( Has (..)
  , grab
  ) where

import Relude


class Has field env where
  obtain :: env -> field

grab :: forall field env m . (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field