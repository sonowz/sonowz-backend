module Sonowz.Mp3tagAutofix.App
  ( runMainFn
  ) where

import Sonowz.Mp3tagAutofix.Imports

import Data.List ((\\))
import Sonowz.Core.HTTP.Effect (HTTP, HttpException, runHTTPIO)
import Sonowz.Core.Time.Effect (Time, timeToIO)
import Sonowz.Mp3tagAutofix.AudioTag.Autofix.Logic (makeArtistFixes, makeArtistPool, runSearches)
import Sonowz.Mp3tagAutofix.AudioTag.Autofix.Parser (ParseException)
import Sonowz.Mp3tagAutofix.AudioTag.Types (AudioTag(..))
import Sonowz.Mp3tagAutofix.AudioTagIO.Effect
  (AudioTagIO, HTagLibException, readAudioTag, runAudioTagIOIO, writeAudioTag)
import Sonowz.Mp3tagAutofix.Env (Env(..))
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Text.Pretty.Simple (OutputOptions(..), defaultOutputOptionsNoColor, pShowOpt)


-- TODO: override 'Polysemy.Error' interpreters to have catch functions

runMainFn :: Env -> IO ()
runMainFn env =
  mainFn
    & runReader env
    & runHTTPIO
    & mapError @HttpException toException
    & runAudioTagIOIO
    & mapError @HTagLibException toException
    & (mapError @ParseException toException . flip (catch @ParseException) (error . show))
    & timeToIO
    & embedToFinal
    & stdEffToIOFinal
    & runFinal @IO

type MainEfffects
  = Embed IO
  : Reader Env
  : Time
  : HTTP
  : AudioTagIO
  : Error ParseException
  : Error HTagLibException
  : StdEff

mainFn :: (Members MainEfffects r, HasCallStack) => Sem r ()
mainFn = do
  env         <- ask
  targetFiles <- getTargetFileList (targetDir env)
  logInfo $ "Found " <> show (length targetFiles) <> " files in " <> toText (targetDir env) <> "."
  logInfo "Extracting ID3 tags..."
  audioTags <- catMaybes <$> mapM readAudioTagWrapped targetFiles
  logInfo $ "Extracted " <> show (length audioTags) <> " ID3 tags."

  let
    artistPool = makeArtistPool audioTags
    ac         = show (length artistPool)
  logInfo $ "Run search for " <> ac <> " unique artist names... (ETA: " <> ac <> " seconds)"
  artistPool' <- runSearches artistPool
  let dropped = show (length artistPool - length artistPool')
  logInfo $ "Search done. " <> dropped <> " artists were dropped."

  let fixes = makeArtistFixes artistPool'
  logInfo "List of fixes:"
  let
    showOpt =
      defaultOutputOptionsNoColor { outputOptionsPageWidth = 100, outputOptionsCompact = True }
  logInfo $ toStrict $ pShowOpt showOpt fixes
  pass


readAudioTagWrapped
  :: Members '[AudioTagIO , Error HTagLibException , StdLog] r => FilePath -> Sem r (Maybe AudioTag)
readAudioTagWrapped file = catch
  (Just <$> readAudioTag file)
  (\_ -> do
    logError $ "Read ID3 tag for " <> toText file <> " failed."
    return Nothing
  )


getTargetFileList :: Members '[Embed IO , Error SomeException] r => FilePath -> Sem r [FilePath]
getTargetFileList dir = do
  liftIO $ ifM (doesDirectoryExist dir) pass (fail $ "No directory named " <> dir)
  dirContents         <- liftIO $ (dir </>) <<$>> listDirectory dir
  (subdirs, dirFiles) <- liftIO $ partitionM doesDirectoryExist dirContents
  subdirFiles         <- join <$> mapM getTargetFileList subdirs
  return (dirFiles <> subdirFiles) where
  partitionM f l = makePartition l <$> filterM f l
  makePartition l trues = (trues, l \\ trues)

