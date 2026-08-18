module Sonowz.Core.LLM.Gemini
  ( defaultContentRequest,
    withGoogleSearchTool,
    withResponseSchema,
    googleSearchTool,
  )
where

import Data.Aeson (toJSON)
import Data.Aeson.Types (emptyObject)
import Data.OpenApi (ToSchema, toSchema)
import Sonowz.Core.Imports
import Sonowz.Core.LLM.Gemini.Types

defaultContentRequest :: Text -> Text -> GenerateContentRequest
defaultContentRequest systemPrompt userPrompt =
  GenerateContentRequest
    { contents = [textContent userPrompt],
      tools = Nothing,
      toolConfig = Nothing,
      safetySettings = Just defaultSafetySettings,
      systemInstruction = Just (textContent systemPrompt),
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
defaultSafetySettings = toNone <$> allCategories
  where
    toNone category = SafetySetting category BlockNone
    allCategories = [minBound .. maxBound] :: [SafetyCategory]

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
