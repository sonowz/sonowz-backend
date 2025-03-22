module Sonowz.Rag.Web.Rag
  ( RagAPI,
    ragAPIHandler,
  )
where

import Data.Aeson (FromJSON)
import Data.Aeson.Types (ToJSON)
import Servant
import Sonowz.Core.DB.Pool (DBEffects)
import Sonowz.Core.Exception.Types (ParseException)
import Sonowz.Core.HTTP.Effect (HTTP)
import Sonowz.Rag.Env (Env)
import Sonowz.Rag.Imports
import Sonowz.Rag.Rag (doRagSearch)
import Sonowz.Rag.Rag.Types (RagResultDocument)

type RagAPI = "rag" :> ReqBody '[JSON] RagRequest :> Post '[JSON] RagResponse

newtype RagRequest = RagRequest
  { query :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

newtype RagResponse = RagResponse
  { result :: [RagResultDocument]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

type RagAPIEffects =
  Reader Env
    : HTTP
    : Error ParseException
    : DBEffects

ragAPIHandler :: forall r. Members RagAPIEffects r => ServerT RagAPI (Sem r)
ragAPIHandler request = RagResponse <$> doRagSearch (query request)