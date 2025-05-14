module Sonowz.Rag.Web.Document
  ( DocumentAPI,
    documentAPIHandler,
  )
where

import Servant
import Sonowz.Core.DB.Field (Uid)
import Sonowz.Core.DB.Pool (DBEffects)
import Sonowz.Core.Web.CRUD (CRUDAPI, crudHandlerFromDBQueries)
import Sonowz.Rag.Document.DB.Queries (rawDocumentCRUD)
import Sonowz.Rag.Document.DB.Types (RawDocument, RawDocumentWriteDto)
import Sonowz.Rag.Imports

type DocumentAPI = CRUDAPI Uid RawDocumentWriteDto RawDocument "document"

documentAPIHandler :: (Member (Error ServerError) r, Members DBEffects r) => ServerT DocumentAPI (Sem r)
documentAPIHandler = crudHandlerFromDBQueries rawDocumentCRUD
