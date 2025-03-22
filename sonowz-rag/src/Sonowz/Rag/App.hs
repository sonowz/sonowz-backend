module Sonowz.Rag.App
  ( server,
    runWithEffects,
    RagServerAPI,
  )
where

import Polysemy.Resource (resourceToIOFinal)
import Servant
import Sonowz.Core.DB.Pool (DBEffects)
import Sonowz.Core.Error.Effect (mapErrorAs500)
import Sonowz.Core.Exception.Types (ParseException)
import Sonowz.Core.HTTP.Effect (HTTP, HttpException, runHTTPIO)
import Sonowz.Rag.Env (Env (..))
import Sonowz.Rag.Imports
import Sonowz.Rag.Web.EmbeddingGeneration (EmbeddingGenerationAPI, embeddingGenerationAPIHandler)
import Sonowz.Rag.Web.Rag (RagAPI, ragAPIHandler)

type RagServerAPI = EmbeddingGenerationAPI :<|> RagAPI

type RagHandlerEffects =
  [ Reader Env,
    HTTP,
    Error ServerError,
    Error ParseException
  ]
    <> DBEffects

server :: Members RagHandlerEffects r => ServerT RagServerAPI (Sem r)
server = embeddingGenerationAPIHandler :<|> ragAPIHandler

runWithEffects :: forall a. Env -> Sem _ a -> Handler a
runWithEffects env (action :: Members RagHandlerEffects r => Sem r a) =
  action
    & runReader env
    & runReader (envPgConnection env)
    & runHTTPIO
    & mapErrorAs500 @HttpException
    & mapErrorAs500 @ParseException
    & embedToFinal @IO
    & resourceToIOFinal
    & stdEffToWebHandler