module Sonowz.Raytrace.Core.Has
    ( Has(..)
    , MonadHas(..)
    )
where

import Relude


class Has field env where
  obtain :: env -> field

{- 
instance a ~ a' => Has a a' where
  obtain = id
 -}

class MonadHas field m where
  grab :: m field

-- MonadReader has fundep "m -> env", thus this instance is decidable
{- instance (MonadReader env m, Has field env) => MonadHas field m where
  grab = asks $ obtain @field
 -}
{- instance (Monad m, Has field r) => MonadHas field (ReaderT r m) where
  grab = asks $ obtain @field
 -}
