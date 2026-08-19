{-# LANGUAGE TemplateHaskell #-}

module Sonowz.Core.Llm.Effect
  ( LlmRequest (..),
    LlmEnv (..),
    Llm,
    LlmException,
    generateTextWithLlm,
    generateStructuredDataWithLlm,
    runHttpGemini,
  )
where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.OpenApi (ToSchema)
import Data.Text qualified as T
import Network.HTTP.Client (Request (..), RequestBody (RequestBodyLBS), parseRequest)
import Sonowz.Core.Http.Effect (Http, fetchWithRequest)
import Sonowz.Core.Imports
import Sonowz.Core.Llm.Gemini (defaultContentRequest, parseResponse, withGoogleSearchTool, withResponseSchema)
import Sonowz.Core.Llm.Gemini.Types (GenerateContentRequest)

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

newtype LlmException = LlmException Text
  deriving (Show)
  deriving anyclass (Exception)

data Llm m a where
  GenerateTextWithLlm :: LlmRequest -> Llm m Text
  GenerateStructuredDataWithLlm :: (FromJSON a, ToSchema a) => Proxy a -> LlmRequest -> Llm m a

makeSem ''Llm

llmRequestToGeminiRequest :: LlmRequest -> GenerateContentRequest
llmRequestToGeminiRequest LlmRequest {userPrompt, systemPrompt, enableSearch} =
  let req = defaultContentRequest systemPrompt userPrompt
   in if enableSearch then withGoogleSearchTool req else req

runHttpGemini ::
  (Members '[Http, Reader LlmEnv, Error LlmException] r) =>
  Sem (Llm : r) a ->
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
    mapParseException :: (ToText a, Member (Error LlmException) r) => Text -> Either a b -> Sem r b
    mapParseException response = \case
      Left errMsg -> throw $ LlmException $ "Failed to parse Gemini response: " <> toText errMsg <> " | Response text: " <> response
      Right val -> pure val

makeGeminiRequest :: Text -> Text -> GenerateContentRequest -> Either LlmException Request
makeGeminiRequest apiKey model bodyDto = do
  let urlString = "https://generativelanguage.googleapis.com/v1beta/models/" <> model <> ":generateContent?key=" <> apiKey
  baseReq <- first (const $ LlmException "Failed to parse Gemini API URL") (parseRequest (T.unpack urlString))
  pure
    $ baseReq
      { method = "POST",
        requestHeaders =
          [ ("Content-Type", "application/json")
          ],
        requestBody = RequestBodyLBS (encode bodyDto)
      }
