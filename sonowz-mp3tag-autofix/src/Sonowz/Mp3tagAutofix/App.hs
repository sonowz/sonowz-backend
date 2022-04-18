module Sonowz.Mp3tagAutofix.App
  ( runMainFn
  ) where

import Sonowz.Mp3tagAutofix.Imports

import Data.List ((\\))
import Sonowz.Core.HTTP.Effect (HTTP, HttpException, runHTTPIO)
import Sonowz.Core.Time.Effect (Time, timeToIO)
import Sonowz.Mp3tagAutofix.AudioTag.Autofix.Logic (makeArtistFixes, makeArtistPool, runSearches)
import Sonowz.Mp3tagAutofix.AudioTag.Autofix.Parser (ParseException)
import Sonowz.Mp3tagAutofix.AudioTag.Types (Artist, AudioTag(..), Encoding(EncodingUtf8), unArtist)
import Sonowz.Mp3tagAutofix.AudioTagIO.Effect
  (AudioTagIO, HTagLibException, readAudioTag, runAudioTagIOIO, writeAudioTag)
import Sonowz.Mp3tagAutofix.Env (Env(..))
import Sonowz.Mp3tagAutofix.Fix.Interactive (interactiveFilterFix)
import Sonowz.Mp3tagAutofix.Fix.Types
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))


newtype MainException = MainException String deriving (Show, Exception)

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
  if nonInteractive env then logWarning "This will automatically update tags in the file!" else pass

  logInfo $ "Found " <> show (length targetFiles) <> " files in " <> toText (targetDir env) <> "."
  logInfo "Extracting ID3 tags..."
  !audioTags <- catMaybes <$> mapM readAudioTagWrapped targetFiles
  logInfo $ "Extracted " <> show (length audioTags) <> " ID3 tags."

  logInfo
    $  "Set encoding of tags to UTF-8 in the files? "
    <> "This might result in some broken encoding in files! (yes or no):"
  whenM getYesOrNo (applyEncodingFixes audioTags)

  let
    artistPool = makeArtistPool audioTags
    ac         = show (length artistPool)
  logInfo $ "Run search for " <> ac <> " unique artist names... (ETA: " <> ac <> " seconds)"
  !artistPool' <- runSearches artistPool
  let dropped = length artistPool - length artistPool'
  logInfo
    $  "Search done. "
    <> (if dropped == 0 then mempty else show dropped <> " artists were dropped.")

  let fixes = makeArtistFixes artistPool'
  !fixes <- if nonInteractive env then return fixes else interactiveFilterFix fixes unArtist

  logInfo "Update tags in the files? (yes or no):"
  whenM getYesOrNo (applyArtistFixes audioTags fixes)
  logInfo "All done."
 where
  getYesOrNo :: Members (Reader Env : Embed IO : StdEff) r => Sem r Bool
  getYesOrNo = do
    env <- ask
    if nonInteractive env
      then return True  -- noninteractive mode always returns True
      else liftIO getLine >>= \case
        "yes" -> return True
        "no"  -> return False
        _     -> logInfo "Please type either \"yes\" or \"no\":" >> getYesOrNo


type AudioTagIOEffects = '[AudioTagIO , Error HTagLibException , StdLog]

readAudioTagWrapped
  :: (Members AudioTagIOEffects r, HasCallStack) => FilePath -> Sem r (Maybe AudioTag)
readAudioTagWrapped file = catch
  (Just <$> readAudioTag file)
  (\_ -> do
    logError $ "Read ID3 tag for " <> toText file <> " failed."
    return Nothing
  )

writeAudioTagWrapped :: (Members AudioTagIOEffects r, HasCallStack) => AudioTag -> Sem r ()
writeAudioTagWrapped tag = catch
  (writeAudioTag tag)
  (\_ -> logError $ "Write ID3 tag for " <> toText (filename tag) <> " failed.")


applyArtistFixes
  :: (Members AudioTagIOEffects r, HasCallStack) => [AudioTag] -> Fix Artist -> Sem r ()
applyArtistFixes audioTags fixes = do
  let
    fixed   = (\tag -> tag { artist = applyFix fixes (artist tag) }) <$> audioTags
    changed = catMaybes
      $ zipWith (\orig fixed -> if orig /= fixed then Just fixed else Nothing) audioTags fixed
  logInfo $ show (length changed) <> " files will be written."
  mapM_
    (\tag -> do
      logDebug $ "Writing tag to " <> toText (filename tag) <> "..."
      writeAudioTagWrapped tag
    )
    changed
  logInfo "All tags were updated."


applyEncodingFixes :: (Members AudioTagIOEffects r, HasCallStack) => [AudioTag] -> Sem r ()
applyEncodingFixes audioTags = do
  let
    changed =
      mapMaybe (\tag -> if encoding tag /= EncodingUtf8 then Just tag else Nothing) audioTags
  logInfo $ show (length changed) <> " files will be written."
  mapM_
    (\tag -> do
      logDebug $ "Writing tag to " <> toText (filename tag) <> "..."
      writeAudioTagWrapped tag
    )
    changed
  logInfo "All tags were updated."


getTargetFileList :: Members (Embed IO : StdEff) r => FilePath -> Sem r [FilePath]
getTargetFileList dir = do
  unlessM
    (liftIO (doesDirectoryExist dir))
    (let errorMsg = "No directory named " <> toText dir
     in logError errorMsg >> throw' (MainException $ toString errorMsg)
    )
  dirContents         <- liftIO $ (dir </>) <<$>> listDirectory dir
  (subdirs, dirFiles) <- liftIO $ partitionM doesDirectoryExist dirContents
  subdirFiles         <- join <$> mapM getTargetFileList subdirs
  return (dirFiles <> subdirFiles) where
  partitionM f l = makePartition l <$> filterM f l
  makePartition l trues = (trues, l \\ trues)
