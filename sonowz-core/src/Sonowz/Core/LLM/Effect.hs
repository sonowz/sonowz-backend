{-# LANGUAGE TemplateHaskell #-}

module Sonowz.Core.LLM.Effect
  ( LlmRequest (..),
    LlmEnv (..),
    LLM,
    LLMException,
    generateTextWithLlm,
    generateStructuredDataWithLlm,
    runHttpGemini,
  )
where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.OpenApi (ToSchema)
import Data.Text qualified as T
import Network.HTTP.Client (Request (..), RequestBody (RequestBodyLBS), parseRequest)
import Sonowz.Core.HTTP.Effect (HTTP, fetchWithRequest)
import Sonowz.Core.Imports
import Sonowz.Core.LLM.Gemini (defaultContentRequest, parseResponse, withGoogleSearchTool, withResponseSchema)
import Sonowz.Core.LLM.Gemini.Types (GenerateContentRequest)

data LlmRequest = LlmRequest
  { userPrompt :: Text,
    systemPrompt :: Maybe Text,
    enableSearch :: Bool
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data LlmEnv = LlmEnv
  { apiKey :: Text,
    modelName :: Text
  }
  deriving (Show)

newtype LLMException = LLMException Text
  deriving (Show)
  deriving anyclass (Exception)

data LLM m a where
  GenerateTextWithLlm :: LlmRequest -> LLM m Text
  GenerateStructuredDataWithLlm :: (FromJSON a, ToSchema a) => Proxy a -> LlmRequest -> LLM m a

makeSem ''LLM

llmRequestToGeminiRequest :: LlmRequest -> GenerateContentRequest
llmRequestToGeminiRequest LlmRequest {userPrompt, systemPrompt, enableSearch} =
  let req = defaultContentRequest systemPrompt userPrompt
   in if enableSearch then withGoogleSearchTool req else req

runHttpGemini ::
  (Members '[HTTP, Reader LlmEnv, Error LLMException] r) =>
  Sem (LLM : r) a ->
  Sem r a
runHttpGemini = interpret $ \case
  GenerateTextWithLlm llmRequest -> do
    LlmEnv {apiKey, modelName} <- ask
    let requestDto = llmRequestToGeminiRequest llmRequest
    request <- fromEither $ makeGeminiRequest apiKey modelName requestDto
    response <- fetchWithRequest request
    responseDto <- mapParseException response $ eitherDecode (encodeUtf8 response)
    mapParseException response $ parseResponse responseDto
  GenerateStructuredDataWithLlm responseTypeProxy llmRequest -> do
    LlmEnv {apiKey, modelName} <- ask
    let requestDto = withResponseSchema responseTypeProxy $ llmRequestToGeminiRequest llmRequest
    request <- fromEither $ makeGeminiRequest apiKey modelName requestDto
    response <- fetchWithRequest request
    responseDto <- mapParseException response $ eitherDecode (encodeUtf8 response)
    jsonText <- mapParseException response $ parseResponse responseDto
    mapParseException response $ eitherDecode (encodeUtf8 jsonText)
  where
    mapParseException :: (ToText a, Member (Error LLMException) r) => Text -> Either a b -> Sem r b
    mapParseException response = \case
      Left errMsg -> throw $ LLMException $ "Failed to parse Gemini response: " <> toText errMsg <> " | Response text: " <> response
      Right val -> pure val

makeGeminiRequest :: Text -> Text -> GenerateContentRequest -> Either LLMException Request
makeGeminiRequest apiKey model bodyDto = do
  let urlString = "https://generativelanguage.googleapis.com/v1beta/models/" <> model <> ":generateContent?key=" <> apiKey
  baseReq <- first (const $ LLMException "Failed to parse Gemini API URL") (parseRequest (T.unpack urlString))
  pure $
    baseReq
      { method = "POST",
        requestHeaders =
          [ ("Content-Type", "application/json")
          ],
        requestBody = RequestBodyLBS (encode bodyDto)
      }
