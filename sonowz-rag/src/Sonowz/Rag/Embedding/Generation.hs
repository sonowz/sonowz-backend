module Sonowz.Rag.Embedding.Generation
  ( generateMissingEmbeddings,
  )
where

import Sonowz.Core.DB.Pool (DBEffects, withDBConn)
import Sonowz.Core.Exception.Types (ParseException)
import Sonowz.Core.HTTP.Effect (HTTP)
import Sonowz.Rag.Document.Types (document, title, uid)
import Sonowz.Rag.Embedding.Generation.OpenAI (createOpenAIEmbedding3)
import Sonowz.Rag.Embedding.Generation.Queries (openAI3EmbeddingTableName)
import Sonowz.Rag.Embedding.Generation.Queries qualified as Queries
import Sonowz.Rag.Env (Env)
import Sonowz.Rag.Imports

generateMissingEmbeddings :: (Members '[Reader Env, HTTP, Error ParseException] r, Members DBEffects r) => Sem r ()
generateMissingEmbeddings = do
  logDebug "Checking for missing embeddings..."
  documentsMissingEmbeddings <-
    withDBConn (\conn -> liftIO $ Queries.selectDocumentsWithoutEmbedding conn openAI3EmbeddingTableName)
  logInfo $ "Generating embeddings for " <> show (length documentsMissingEmbeddings) <> " documents..."
  for_ documentsMissingEmbeddings $ \docEntity -> do
    logInfo $ "Generating embedding for document " <> show (title docEntity) <> "..."
    embedding <- createOpenAIEmbedding3 (document docEntity)
    withDBConn (\conn -> liftIO $ Queries.createEmbedding conn openAI3EmbeddingTableName (uid docEntity) embedding)
  logInfo "Done generating embeddings."