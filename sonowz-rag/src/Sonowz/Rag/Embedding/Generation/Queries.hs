module Sonowz.Rag.Embedding.Generation.Queries
  ( openAI3EmbeddingTableName,
    createEmbedding,
    selectDocumentsWithoutEmbedding,
  )
where

import Data.Vector (Vector)
import Database.PostgreSQL.Simple
import Sonowz.Core.DB.Field (Uid)
import Sonowz.Rag.Document.Types (RawDocument)
import Sonowz.Rag.Imports

{-
CREATE TABLE public.rag_raw_document (
    uid serial PRIMARY KEY NOT NULL,
    document_id character varying(255) NOT NULL UNIQUE,
    source text NOT NULL,
    title text NOT NULL,
    document text NOT NULL,
    created_time timestamp with time zone NOT NULL DEFAULT now(),
    updated_time timestamp with time zone NOT NULL DEFAULT now()
);

CREATE TABLE public.rag_embedding_openai_3 (
    uid serial PRIMARY KEY NOT NULL,
    document_uid integer NOT NULL,
    embedding vector(3072) NOT NULL,
    CONSTRAINT fk_rag_raw_document FOREIGN KEY (document_uid)
        REFERENCES public.rag_raw_document(uid)
        ON DELETE CASCADE
);
-}

openAI3EmbeddingTableName :: Text
openAI3EmbeddingTableName = "rag_embedding_openai_3"

createEmbedding :: Connection -> Text -> Uid -> Vector Float -> IO Bool
createEmbedding conn tableName documentUid embedding =
  let sql = fromString . toString $ "INSERT INTO " <> tableName <> " (uid, document_uid, embedding) VALUES (DEFAULT, ?, ?)"
   in (==) 1 <$> execute conn sql (documentUid, embedding)

-- Use 'fold' instead of 'query' if memory exceeds
selectDocumentsWithoutEmbedding :: Connection -> Text -> IO [RawDocument]
selectDocumentsWithoutEmbedding conn tableName =
  let sql = fromString . toString $ "SELECT * FROM rag_raw_document WHERE uid NOT IN (SELECT DISTINCT document_uid FROM " <> tableName <> ")"
   in query_ conn sql
