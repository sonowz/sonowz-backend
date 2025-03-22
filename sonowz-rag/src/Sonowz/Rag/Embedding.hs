module Sonowz.Rag.Embedding
  ( generateMissingEmbeddings,
  )
where

import Sonowz.Core.DB.Pool (DBEffects, withDBConn)
import Sonowz.Core.Exception.Types (ParseException (..))
import Sonowz.Core.HTTP.Effect (HTTP)
import Sonowz.Rag.Document.Types (document, title, uid)
import Sonowz.Rag.Embedding.OpenAI (createOpenAIEmbedding3)
import Sonowz.Rag.Embedding.Queries qualified as Queries
import Sonowz.Rag.Env (Env)
import Sonowz.Rag.Imports

-- TODO: chunk the document
-- TODO: add title to the request
generateMissingEmbeddings :: (Members '[Reader Env, HTTP, Error ParseException] r, Members DBEffects r) => Sem r ()
generateMissingEmbeddings = do
  logDebug "Checking for missing embeddings..."
  documentsMissingEmbeddings <- withDBConn (\conn -> liftIO $ Queries.selectDocumentsWithoutEmbedding conn Queries.openAI3EmbeddingTableName)
  logInfo $ "Generating embeddings for " <> show (length documentsMissingEmbeddings) <> " documents..."
  for_ documentsMissingEmbeddings $ \docEntity -> do
    logInfo $ "Generating embedding for document " <> show (title docEntity) <> "..."
    embedding <- createOpenAIEmbedding3 (document docEntity)
    created <- withDBConn (\conn -> liftIO $ Queries.insertEmbedding conn Queries.openAI3EmbeddingTableName (uid docEntity) embedding)
    unless created $ throw $ ParseException "Failed to insert embedding to DB"
  logInfo "Done generating embeddings."