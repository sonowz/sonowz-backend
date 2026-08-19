{-# LANGUAGE OverloadedRecordDot #-}

module Sonowz.Core.LLM.Gemini
  ( defaultContentRequest,
    withGoogleSearchTool,
    withResponseSchema,
    googleSearchTool,
    parseResponse,
  )
where

import Data.Aeson (toJSON)
import Data.Aeson.Types (emptyObject)
import Data.OpenApi (ToSchema, toSchema)
import Data.Text qualified as T
import Sonowz.Core.Imports
import Sonowz.Core.LLM.Gemini.Types

defaultContentRequest :: Maybe Text -> Text -> GenerateContentRequest
defaultContentRequest systemPrompt userPrompt =
  GenerateContentRequest
    { contents = [textContent userPrompt],
      tools = Nothing,
      toolConfig = Nothing,
      safetySettings = Just defaultSafetySettings,
      systemInstruction = fmap textContent systemPrompt,
      generationConfig = Just defaultGenerationConfig,
      cachedContent = Nothing,
      serviceTier = Nothing,
      store = Nothing
    }
  where
    textContent :: Text -> Content
    textContent text =
      Content
        { parts =
            [ Part
                { text = Just text,
                  functionCall = Nothing,
                  functionResponse = Nothing,
                  fileData = Nothing,
                  thought = Nothing
                }
            ],
          role = Nothing
        }

defaultSafetySettings :: [SafetySetting]
defaultSafetySettings = toNone <$> validCategories
  where
    toNone category = SafetySetting category BlockNone
    validCategories = filter (/= HarmCategoryUnspecified) [minBound .. maxBound]

defaultGenerationConfig :: GenerationConfig
defaultGenerationConfig =
  GenerationConfig
    { temperature = Nothing,
      topP = Nothing,
      topK = Nothing,
      candidateCount = Nothing,
      maxOutputTokens = Nothing,
      stopSequences = Nothing,
      responseMimeType = Nothing,
      responseSchema = Nothing,
      responseModalities = Nothing,
      seed = Nothing,
      presencePenalty = Nothing,
      frequencyPenalty = Nothing,
      responseLogprobs = Nothing,
      logprobs = Nothing,
      thinkingConfig = Nothing,
      speechConfig = Nothing,
      imageConfig = Nothing,
      mediaResolution = Nothing,
      enableEnhancedCivicAnswers = Nothing,
      enableAffectiveDialog = Nothing,
      responseFormat = Nothing,
      translationConfig = Nothing
    }

withGoogleSearchTool :: GenerateContentRequest -> GenerateContentRequest
withGoogleSearchTool req =
  req
    { tools = Just [googleSearchTool]
    }

googleSearchTool :: Tool
googleSearchTool =
  Tool
    { functionDeclarations = Nothing,
      codeExecution = Nothing,
      googleSearch = Just emptyObject
    }

withResponseSchema :: forall a. (ToSchema a) => Proxy a -> GenerateContentRequest -> GenerateContentRequest
withResponseSchema proxy req =
  req
    { generationConfig = Just newGenerationConfig
    }
  where
    formatConfig =
      ResponseFormatConfig
        { text = Just textFormat,
          audio = Nothing,
          image = Nothing
        }
    textFormat =
      TextResponseFormat
        { mimeType = Just ApplicationJson,
          schema = Just $ toJSON (toSchema proxy)
        }
    newGenerationConfig =
      (generationConfig req ?: defaultGenerationConfig)
        { responseFormat = Just formatConfig
        }

parseCandidate :: Candidate -> Either Text Text
parseCandidate Candidate {content = Just Content {parts = parts}} =
  let texts = mapMaybe (\p -> p.text) parts
   in if null texts then Left "No text found in candidate" else Right (T.concat texts)
parseCandidate _ = Left "Invalid candidate format"

parseResponse :: GenerateContentResponse -> Either Text Text
parseResponse GenerateContentResponse {candidates = Just (c : _)} = parseCandidate c
parseResponse _ = Left "No candidates in response"