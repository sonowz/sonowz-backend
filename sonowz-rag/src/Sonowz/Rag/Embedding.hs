module Sonowz.Rag.Embedding
  ( generateMissingEmbeddings,
  )
where

import Control.Monad (foldM)
import Data.Text qualified as T
import Sonowz.Core.DB.Pool (DBEffects, withDBConn)
import Sonowz.Core.Exception.Types (ParseException (..))
import Sonowz.Core.HTTP.Effect (HTTP)
import Sonowz.Rag.Document.Types (RawDocument, document, title, uid)
import Sonowz.Rag.Embedding.DB.Queries qualified as Queries
import Sonowz.Rag.Embedding.OpenAI (createOpenAIEmbedding3)
import Sonowz.Rag.Env (Env)
import Sonowz.Rag.Imports

generateMissingEmbeddings ::
  ( Members '[Reader Env, HTTP, Error ParseException] r,
    Members DBEffects r,
    HasCallStack
  ) =>
  Sem r ()
generateMissingEmbeddings = do
  logDebug "Checking for missing embeddings..."
  documentsMissingEmbeddings <- withDBConn (\conn -> liftIO $ Queries.selectDocumentsWithoutEmbedding conn Queries.openAI3EmbeddingTableName)
  logInfo $ "Generating embeddings for " <> show (length documentsMissingEmbeddings) <> " documents..."
  for_ documentsMissingEmbeddings $ \docEntity -> do
    logInfo $ "Generating embedding for document " <> show (title docEntity) <> "..."
    let chunks = prepareDocument docEntity
    when (length chunks > 1) (logDebug $ "Document was split into " <> show (length chunks) <> " chunks.")
    for_ chunks $ \chunk -> do
      embedding <- createOpenAIEmbedding3 chunk
      created <- withDBConn (\conn -> liftIO $ Queries.insertEmbedding conn Queries.openAI3EmbeddingTableName (uid docEntity) embedding)
      unless created $ throw $ ParseException "Failed to insert embedding to DB"
  logInfo "Done generating embeddings."

prepareDocument :: RawDocument -> [Text]
prepareDocument doc = addTitle <$> chunks
  where
    -- Chunk size can exceed this when the title is too long
    chunkMaxSize = 4096
    halvedChunkMaxSize = chunkMaxSize `div` 2
    chunks =
      tryChunkOn "\r\n\r\n"
        <|> tryChunkOn "\n\n"
        <|> tryChunkOn "\r\n"
        <|> tryChunkOn "\n"
        ?: plainChunks
    tryChunkOn :: Text -> Maybe [Text]
    tryChunkOn sep = chunks
      where
        splits = T.splitOn sep (document doc) :: [Text]
        halfChunks = reverse <$> foldM (trySplitToHalfChunk sep) [] splits :: Maybe [Text]
        chunks = halfChunkToChunks <$> halfChunks :: Maybe [Text]
    trySplitToHalfChunk :: Text -> [Text] -> Text -> Maybe [Text]
    trySplitToHalfChunk _ [] split = guard (T.length split <= halvedChunkMaxSize) $> [split]
    trySplitToHalfChunk sep (buffer : done) split =
      guard (T.length split <= chunkMaxSize)
        $> if T.length newBuffer <= chunkMaxSize
          then newBuffer : done
          else split : buffer : done
      where
        newBuffer = buffer <> sep <> split
    halfChunkToChunks :: [Text] -> [Text]
    halfChunkToChunks [chunk] = [chunk]
    halfChunkToChunks halfChunks = zipWith (<>) halfChunks (drop 1 halfChunks)
    plainChunks = halfChunkToChunks (T.chunksOf halvedChunkMaxSize (document doc))

    addTitle chunk = "Title: " <> title doc <> "\n\n" <> chunk
