module Sonowz.NewsCombinator.Rule.Executor
  ( evalNewsScrapRule,
  )
where

import Sonowz.Core.Llm.Effect (Llm, LlmRequest (..), generateStructuredDataWithLlm)
import Sonowz.NewsCombinator.Imports
import Sonowz.NewsCombinator.News.Types (LlmEvaluationResult (..))
import Sonowz.NewsCombinator.Rule.Types (ConfidenceLevel (..), NewsScrapRule (..))

evalNewsScrapRule ::
  (Members (Llm : StdEff) r) =>
  NewsScrapRule ->
  Sem r (Maybe LlmEvaluationResult, NewsScrapRule)
evalNewsScrapRule rule = do
  let request =
        LlmRequest
          { userPrompt = rule.description,
            systemPrompt = Just $ buildSystemPrompt rule.confidenceLevel,
            enableSearch = True
          }

  evalResult <- generateStructuredDataWithLlm (Proxy @LlmEvaluationResult) request
  if evalResult.isMatch
    then return (Just evalResult, updateRule rule)
    else return (Nothing, rule)
  where
    updateRule r
      | r.isOneTimeRule = r {isEnabled = False}
      | otherwise = r

buildSystemPrompt :: ConfidenceLevel -> Text
buildSystemPrompt confidence =
  "You are a news monitoring agent. Your task is to perform web search and check if there is recent news matching the user's request.\n"
    <> "Confidence requirement level: "
    <> show confidence
    <> ".\n"
    <> "- Official: Only consider news from official announcements, verified sources, or reliable press.\n"
    <> "- Rumor: Consider news including rumors, leaks, speculation, or unconfirmed reports as well as official news.\n"
    <> "Evaluate whether there is matching news. "
    <> "Set `isMatch` to true if matching news exists at or above the confidence level, write a concise `summary` of the findings, "
    <> "and list the `matchedArticles` with title, URL link, and publication date."
    <> "If publication date is not available, put today's date."
