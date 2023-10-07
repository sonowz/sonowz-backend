module Sonowz.Core.Exception.Types
  ( NotImplementedException (..),
    ParseException (..),
  )
where

import Sonowz.Core.Imports

newtype NotImplementedException = NotImplemented Text
  deriving (Show)
  deriving anyclass (Exception)

newtype ParseException = ParseException Text
  deriving (Show)
  deriving anyclass (Exception)
