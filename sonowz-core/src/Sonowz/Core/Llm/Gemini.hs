{-# LANGUAGE OverloadedRecordDot #-}

module Sonowz.Core.Llm.Gemini
  ( defaultContentRequest,
    withGoogleSearchTool,
    withResponseSchema,
    googleSearchTool,
    parseResponse,
  )
where

import Data.Aeson (Value (..), toJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (emptyObject)
import Data.HashMap.Strict.InsOrd qualified as InsOrd
import Data.OpenApi (NamedSchema (..), ToSchema, declareNamedSchema)
import Data.OpenApi.Declare (runDeclare)
import Data.Text qualified as T
import Sonowz.Core.Imports
import Sonowz.Core.Llm.Gemini.Types

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
          schema = Just (inlineSchema proxy)
        }
    newGenerationConfig =
      (generationConfig req ?: defaultGenerationConfig)
        { responseFormat = Just formatConfig
        }

-- Gemini API requires response schemas to be fully inlined without any '$ref' pointers.
inlineSchema :: forall a. (ToSchema a) => Proxy a -> Value
inlineSchema proxy = dereferenceValue defsMap rootVal
  where
    (defs, namedSchema) = runDeclare (declareNamedSchema proxy) mempty
    defsMap = KeyMap.fromList [(Key.fromText k, toJSON v) | (k, v) <- InsOrd.toList defs]
    rootVal = toJSON (_namedSchemaSchema namedSchema)

dereferenceValue :: KeyMap.KeyMap Value -> Value -> Value
dereferenceValue defsMap = go
  where
    go (Object kvs)
      | Just (String ref) <- KeyMap.lookup "$ref" kvs =
          let refName = case T.splitOn "/" ref of
                          [] -> ref
                          xs -> viaNonEmpty last xs ?: ref
          in case KeyMap.lookup (Key.fromText refName) defsMap of
               Just resolved -> go resolved
               Nothing -> Object (KeyMap.map go kvs)
      | otherwise = Object (KeyMap.map go kvs)
    go (Array arr) = Array (fmap go arr)
    go other = other

parseCandidate :: Candidate -> Either Text Text
parseCandidate Candidate {content = Just Content {parts = parts}} =
  let texts = mapMaybe (\p -> p.text) parts
   in if null texts then Left "No text found in candidate" else Right (T.concat texts)
parseCandidate _ = Left "Invalid candidate format"

parseResponse :: GenerateContentResponse -> Either Text Text
parseResponse GenerateContentResponse {candidates = Just (c : _)} = parseCandidate c
parseResponse _ = Left "No candidates in response"