module Sonowz.Rag.Web.EmbeddingGeneration
  ( EmbeddingGenerationAPI,
    embeddingGenerationAPIHandler,
  )
where

import Servant
import Sonowz.Core.DB.Pool (DBEffects)
import Sonowz.Core.Exception.Types (ParseException)
import Sonowz.Core.HTTP.Effect (HTTP)
import Sonowz.Rag.Embedding (generateMissingEmbeddings)
import Sonowz.Rag.Env (Env)
import Sonowz.Rag.Imports

type EmbeddingGenerationAPI = "embedding" :> "generate" :> Post '[JSON] NoContent

type EmbeddingGenerationAPIEffects =
  Reader Env
    : HTTP
    : Error ParseException
    : DBEffects

embeddingGenerationAPIHandler :: Members EmbeddingGenerationAPIEffects r => ServerT EmbeddingGenerationAPI (Sem r)
embeddingGenerationAPIHandler = generateMissingEmbeddings >> return NoContent