module Sonowz.Noti.Imports
  ( module Relude.Applicative
  , module Relude.Base
  , module Relude.Bool
  , module Relude.Container
  , module Relude.Debug
  , module Relude.DeepSeq
  , module Relude.Exception
  , module Relude.File
  , module Relude.Foldable
  , module Relude.Function
  , module Relude.Functor
  , module Relude.Lifted
  , module Relude.List
  , module Relude.Monad
  , module Relude.Monad.Either
  , module Relude.Monad.Maybe
  , module Relude.Monad.Reexport
  , module Relude.Monoid
  , module Relude.Nub
  , module Relude.Numeric
  , module Relude.Print
  , module Relude.String
  , module Polysemy
  , module Polysemy.AtomicState
  , module Polysemy.Error
  , module Polysemy.Reader
  , module Polysemy.State
  , module Sonowz.Core.StdEff.Effect
  , lengthText
  ) where

import Relude.Applicative
import Relude.Base
import Relude.Bool
import Relude.Container
import Relude.Debug
import Relude.DeepSeq
import Relude.Exception hiding (fromException)
import Relude.File
import Relude.Foldable
import Relude.Function
import Relude.Functor
import Relude.Lifted
import Relude.List
import Relude.Monad (chainedTo)
import Relude.Monad.Either
import Relude.Monad.Maybe
import Relude.Monad.Reexport
  hiding
    ( ExceptT(..)
    , MonadReader(..)
    , MonadState(..)
    , Reader
    , ReaderT(..)
    , State
    , StateT(..)
    , asks
    , evalState
    , execState
    , gets
    , modify
    , modify'
    , runReader
    , runState
    , withReader
    , withReaderT
    , withState
    )
-- import Relude.Monad.Trans
import Relude.Monoid
import Relude.Nub
import Relude.Numeric
import Relude.Print
import Relude.String

import Polysemy
import Polysemy.AtomicState
import Polysemy.Error
import Polysemy.Reader
import Polysemy.State

import Sonowz.Core.Instances ()
import Sonowz.Core.StdEff.Effect

import Data.Text as T

lengthText :: Text -> Int
lengthText = T.length
