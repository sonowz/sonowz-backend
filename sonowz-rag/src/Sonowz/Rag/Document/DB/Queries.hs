module Sonowz.Rag.Document.DB.Queries
  ( rawDocumentCRUD,
  )
where

import Opaleye
import Sonowz.Core.DB.CRUD (CRUDQueries, getCRUDQueries)
import Sonowz.Core.DB.Field (Uid)
import Sonowz.Rag.Document.DB.Types

-- Table declarations --

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
-}

rawDocumentTable :: RawDocumentTable
rawDocumentTable = table "rag_raw_document" (pRawDocument fields)
  where
    fields =
      RawDocument'
        { uid = tableField "uid",
          documentId = tableField "document_id",
          source = tableField "source",
          title = tableField "title",
          document = tableField "document",
          createdTime = tableField "created_time",
          updatedTime = tableField "updated_time"
        }

rawDocumentCRUD :: CRUDQueries Uid RawDocumentWriteDto RawDocument
rawDocumentCRUD = getCRUDQueries rawDocumentTable
