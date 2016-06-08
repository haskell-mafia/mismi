{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import           BuildInfo_ambiata_mismi_s3
import           DependencyInfo_ambiata_mismi_s3

import           Data.Conduit
import qualified Data.Conduit.List as DC

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource

import qualified Data.ByteString as BS
import           Data.String (String)
import           Data.Text.IO (putStrLn, hPutStrLn)
import           Data.Text hiding (copy, isPrefixOf, filter)

import           Mismi.Environment
import           Mismi.S3

import           Options.Applicative

import           P

import           System.IO hiding (putStrLn, hPutStrLn)
import           System.Exit
import           System.FilePath
import           System.Posix.Signals
import           System.Posix.Process

import           X.Control.Monad.Trans.Either.Exit
import           X.Control.Monad.Trans.Either (EitherT, eitherT, runEitherT, firstEitherT)
import           X.Options.Applicative

data Recursive =
  Recursive
  | NotRecursive
  deriving (Eq, Show)

rec :: a -> a -> Recursive -> a
rec notrecursive recursive r = case r of
  NotRecursive -> notrecursive
  Recursive -> recursive

data Command =
    Upload FilePath Address WriteMode
  | Download Address FilePath
  | Copy Address Address
  | Move Address Address
  | Exists Address
  | Delete Address
  | Write Address Text WriteMode
  | Read Address
  | Cat Address
  | Size Address
  | Sync Address Address SyncMode Int
  | List Address Recursive
  deriving (Eq, Show)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  forM_ [sigINT, sigTERM, sigQUIT] $ \s ->
    installHandler s (Catch . void . exitImmediately $ ExitFailure 111) Nothing

  dispatch mismi >>= \case
      VersionCommand ->
        putStrLn ("s3: " <> pack buildInfoVersion) >> exitSuccess
      DependencyCommand ->
        mapM (putStrLn . pack)  dependencyInfo >> exitSuccess
      RunCommand DryRun c ->
        print c >> exitSuccess
      RunCommand RealRun c ->
        run c

run :: Command -> IO ()
run c = do
  e <- orDie renderRegionError discoverAWSEnv
  orDie renderError . runAWS e $ case c of
    Upload s d m ->
      uploadWithModeOrFail m s d
    Download s d ->
      renderExit renderDownloadError . download s . optAppendFileName d $ key s
    Copy s d ->
      renderExit renderCopyError $ copy s d
    Move s d ->
      renderExit renderCopyError $ move s d
    Exists a ->
      exists a >>= liftIO . flip unless exitFailure
    Delete a ->
      delete a
    Write a t w ->
      writeWithModeOrFail w a t
    Read a ->
      read a >>= \md -> liftIO $ maybe exitFailure pure md >>= putStrLn
    Cat a ->
      read' a >>= \md -> liftIO $ maybe exitFailure pure md >>= runResourceT . ($$+- DC.mapM_ (liftIO . BS.putStr))
    Size a ->
      getSize a >>= liftIO . maybe exitFailure (putStrLn . pack . show)
    Sync s d m f ->
      renderExit renderSyncError $ syncWithMode m s d f
    List a rq ->
      rec (list' a) (listRecursively' a) rq $$ DC.mapM_ (liftIO . putStrLn . addressToText)

renderExit :: MonadIO m => (e -> Text) -> EitherT e m a -> m a
renderExit f =
  eitherT (\e -> liftIO $ (hPutStrLn stderr $ f e) >> exitFailure) return


optAppendFileName :: FilePath -> Key -> FilePath
optAppendFileName f k = fromMaybe f $ do
  fp <- valueOrEmpty (hasTrailingPathSeparator f || takeFileName f == ".") (takeDirectory f)
  bn <- basename k
  pure . combine fp $ unpack bn

mismi :: Parser (SafeCommand Command)
mismi = safeCommand commandP'

commandP' :: Parser Command
commandP' = subparser $
     command' "upload"
              "Upload a file to s3."
              (Upload <$> filepath' <*> address' <*> writeMode')
  <> command' "download"
              "Download a file from s3."
              (Download <$> address' <*> filepath')
  <> command' "copy"
              "Copy a file from an S3 address to another S3 address."
              (Copy <$> address' <*> address')
  <> command' "move"
              "Move an S3 address to another S3 address"
              (Move <$> address' <*> address')
  <> command' "exists"
              "Check if an address exists."
              (Exists <$> address')
  <> command' "delete"
              "Delete an address."
              (Delete <$> address')
  <> command' "write"
              "Write to an address."
              (Write <$> address' <*> text' <*> writeMode')
  <> command' "read"
              "Read from an address."
              (Read <$> address')
  <> command' "cat"
              "cat from an address."
              (Cat <$> address')
  <> command' "size"
              "Get the size of an address."
              (Size <$> address')
  <> command' "sync"
              "Sync between two prefixes."
              (Sync <$> address' <*> address' <*> syncMode' <*> fork')
  <> command' "ls"
              "Stream a recursively list of objects on a prefixe"
              (List <$> address' <*> recursive')

recursive' :: Parser Recursive
recursive' =
  flag NotRecursive Recursive $
       short 'r'
    <> long "recursive"
    <> help "Recursively list"

address' :: Parser Address
address' = argument (pOption s3Parser) $
     metavar "S3URI"
  <> help "An s3 uri, i.e. s3://bucket/prefix/key"
  <> completer addressCompleter

filepath' :: Parser FilePath
filepath' = strArgument $
     metavar "FILEPATH"
  <> help "Absolute file path, i.e. /tmp/fred"
  <> action "file"

text' :: Parser Text
text' = pack <$> (strArgument $
     metavar "STRING"
  <> help "Data to write to S3 address.")

writeMode' :: Parser WriteMode
writeMode' =
  flag Fail Overwrite $
       long "overwrite"
    <> help "WriteMode"

syncMode' :: Parser SyncMode
syncMode' =
      pure FailSync
  <|> (flag' SkipSync $ long "skip" <> help "Skip over files that already exist in the target location.")
  <|> (flag' OverwriteSync $ long "overwrite" <> help "Overwrite files that already exist in the target location.")

fork' :: Parser Int
fork' = option auto $
     long "fork"
  <> metavar "INT"
  <> help "Number of threads to fork CopyObject call by."
  <> value 8

addressCompleter :: Completer
addressCompleter = mkCompleter $ \s -> do
  res <- runEitherT (go s)
  case res of
    Left   _ -> pure []
    Right xs -> pure xs
  where
    go :: String -> EitherT () IO [String]
    go s = do
      e <- forget discoverAWSEnv
      forget $ runAWS e $ do
        x <- case addressFromText (pack s) of
          Nothing ->
            pure []
          Just a ->
            let a' = withKey dirname a
            in  fmap (unpack . addressToText) <$> list a'
        let x' = filter (isPrefixOf s) x
        pure x'
    forget = firstEitherT (const ())
