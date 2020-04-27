module Sonowz.Raytrace.Core.Has
    ( Has(..)
    , MonadHas(..)
    )
where


class Has field env where
  obtain :: env -> field

class MonadHas field m where
  grab :: m field
