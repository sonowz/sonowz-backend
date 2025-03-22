module Sonowz.Rag.Rag
  ( doRagSearch,
  )
where

import Sonowz.Core.DB.Pool (DBEffects, withDBConn)
import Sonowz.Core.Exception.Types (ParseException)
import Sonowz.Core.HTTP.Effect (HTTP)
import Sonowz.Rag.Document.Types (RawDocument (..), document, title)
import Sonowz.Rag.Embedding.OpenAI (createOpenAIEmbedding3)
import Sonowz.Rag.Embedding.Queries qualified as Queries
import Sonowz.Rag.Env (Env)
import Sonowz.Rag.Imports
import Sonowz.Rag.Rag.Types (RagResultDocument (RagResultDocument))

doRagSearch :: (Members '[Reader Env, HTTP, Error ParseException] r, Members DBEffects r) => Text -> Sem r [RagResultDocument]
doRagSearch query = do
  logDebug "Start RAG..."
  embedding <- createOpenAIEmbedding3 query
  searchedDocuments <- withDBConn (\conn -> liftIO $ Queries.selectTopNDocuments conn Queries.openAI3EmbeddingTableName 5 embedding)
  logDebug $ "RAG result: " <> show (title <$> searchedDocuments)
  pure $ toRagResult <$> searchedDocuments

toRagResult :: RawDocument -> RagResultDocument
toRagResult RawDocument {..} = RagResultDocument title document