module Sonowz.NewsCombinator.News.Types
  ( NewsArticle (..),
    LlmEvaluationResult (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import Sonowz.NewsCombinator.Imports

data NewsArticle = NewsArticle
  { title :: Text,
    link :: Text,
    publishedAt :: UTCTime
  }
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LlmEvaluationResult = LlmEvaluationResult
  { isMatch :: Bool,
    summary :: Text,
    matchedArticles :: [NewsArticle]
  }
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
