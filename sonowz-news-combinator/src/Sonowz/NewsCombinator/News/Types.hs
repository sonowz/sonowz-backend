module Sonowz.NewsCombinator.News.Types
  ( NewsArticle (..),
    LLMEvaluationResult (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)
import Sonowz.NewsCombinator.Imports

data NewsArticle = NewsArticle
  { title :: Text,
    link :: Text,
    publishedAt :: UTCTime
  }
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data LLMEvaluationResult = LLMEvaluationResult
  { isMatch :: Bool,
    summary :: Text,
    matchedArticles :: [NewsArticle]
  }
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
