{-# LANGUAGE QuasiQuotes #-}

module Sonowz.Rag.Embedding.OpenAI
  ( createOpenAIEmbedding3,
  )
where

import Data.Aeson (ToJSON, encode)
import Data.Aeson.Optics
import Data.Text qualified as T
import Data.Vector (Vector)
import Network.HTTP.Client (Request (..), RequestBody (RequestBodyLBS))
import Network.HTTP.Types (hAuthorization, hContentType)
import Optics
import Sonowz.Core.Exception.Types (ParseException (..))
import Sonowz.Core.HTTP.Effect (HTTP, fetchWithRequest, urlToRequest)
import Sonowz.Rag.Embedding.Types (OpenAIKey (getKey))
import Sonowz.Rag.Env (Env (envOpenAIKey))
import Sonowz.Rag.Imports
import URI.ByteString.QQ (uri)

createOpenAIEmbedding3 ::
  ( Members '[Reader Env, HTTP, Error ParseException] r,
    Members StdEff r,
    HasCallStack
  ) =>
  Text ->
  Sem r (Vector Float)
createOpenAIEmbedding3 queryText = do
  when (T.length queryText > 5000) (logWarning "Query text is too long!")
  openAIKey <- envOpenAIKey <$> ask
  request <- fromEither $ openAIEmbeddingRequest (getKey openAIKey) "text-embedding-3-large" 3072 queryText
  response <- fetchWithRequest request
  fromEither $ parseOpenAIEmbeddingResponse response

data OpenAIEmbeddingRequestBody = OpenAIEmbeddingRequestBody
  { input :: Text,
    model :: Text,
    dimensions :: Int
  }
  deriving (Generic, ToJSON)

openAIEmbeddingRequest :: Text -> Text -> Int -> Text -> Either ParseException Request
openAIEmbeddingRequest apiKey model vectorDimension document = do
  let url = [uri|https://api.openai.com/v1/embeddings|]
      requestBodyDto = OpenAIEmbeddingRequestBody {input = document, model = model, dimensions = vectorDimension}
      requestBody = RequestBodyLBS $ encode requestBodyDto
      requestHeaders =
        [ (hContentType, "application/json"),
          (hAuthorization, "Bearer " <> encodeUtf8 apiKey)
        ]
  request <- maybeToRight (ParseException "Failed to parse OpenAI embedding URL") (urlToRequest url)
  return $
    request
      { method = "POST",
        requestHeaders = requestHeaders,
        requestBody = requestBody,
        secure = True
      }

-- Use Optics to parse the response
parseOpenAIEmbeddingResponse :: Text -> Either ParseException (Vector Float)
parseOpenAIEmbeddingResponse response =
  if null parsed
    then Left (ParseException "Failed to parse OpenAI embedding response")
    else Right parsed
  where
    parsed :: Vector Float
    parsed = fromList . toListOf o $ response
    o :: IxFold Int Text Float
    o = key "data" % nth 0 % key "embedding" % values % _Double % to realToFrac
