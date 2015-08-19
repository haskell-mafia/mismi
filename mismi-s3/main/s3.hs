{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

import           BuildInfo_ambiata_mismi_s3

import           Control.Monad.IO.Class

import           Data.Conduit
import qualified Data.Conduit.List as DC
import           Data.Text hiding (copy)

import           Mismi.Environment
import           Mismi.S3

import           Options.Applicative

import           P

import           System.IO
import           System.Exit
import           System.Posix.Signals
import           System.Posix.Process

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
  AwsK AmazonkaCommand
  | AwsC AwsCommand
  deriving (Eq, Show)

data AmazonkaCommand =
  Uploadk FilePath Address
  | Downloadk Address FilePath
  | Sizek Address
  | Synck Address Address SyncMode Int
  | Listk Address
  | Existsk Address
  deriving (Eq, Show)

data AwsCommand =
  List Address Recursive
  | Upload FilePath Address
  | Download Address FilePath
  | Copy Address Address
  | Move Address Address
  | Exists Address
  | Delete Address
  | Write Address Text WriteMode
  | Read Address
  | Size Address
  | Sync Address Address SyncMode Int
  deriving (Eq, Show)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  forM_ [sigINT, sigTERM, sigQUIT] $ \s ->
    installHandler s (Catch . void . exitImmediately $ ExitFailure 111) Nothing

  dispatch mismi >>= \case
      VersionCommand ->
        putStrLn ("s3: " <> buildInfoVersion) >> exitSuccess
      RunCommand DryRun c ->
        print c >> exitSuccess
      RunCommand RealRun c ->
        run c

run :: Command -> IO ()
run = \case
  AwsC c ->
    runC c
  AwsK k ->
    runK k

runK :: AmazonkaCommand -> IO ()
runK k = do
  r' <- getRegionFromEnv
  r <- either (const . pure $ Sydney) pure r'
  orDie awsErrorRender . runAWS r $ case k of
    Uploadk s d ->
      upload s d
    Downloadk s d ->
      download s d
    Sizek a ->
      getSize a >>= liftIO . maybe exitFailure (putStrLn . show)
    Synck s d m f ->
      syncWithMode m s d f
    Listk a ->
      listRecursively' a >>= ($$ DC.mapM_ (liftIO . putStrLn . unpack . addressToText))
    Existsk a ->
      exists a >>= \b -> liftIO $ if b then exitSuccess else exitFailure

runC :: AwsCommand -> IO ()
runC c = do
  r' <- getRegionFromEnv
  r <- either (const . pure $ Sydney) pure r'
  orDie awsErrorRender . runAWS r $ case c of
    List a rq ->
      rec (list a) (listRecursively a) rq >>= liftIO . mapM_ (putStrLn . unpack . addressToText)
    Upload s d ->
      upload s d
    Download s d ->
      download s d
    Copy s d ->
      copy s d
    Move s d ->
      move s d
    Exists a ->
      exists a >>= \b -> liftIO $ if b then exitSuccess else exitFailure
    Delete a ->
      delete a
    Write a t w ->
      writeWithMode w a t
    Read a ->
      read a >>= \md -> liftIO $ maybe exitFailure (pure . unpack) md >>= putStrLn
    Size a ->
      getSize a >>= liftIO . maybe exitFailure (putStrLn . show)
    Sync s d m f ->
      syncWithMode m s d f


mismi :: Parser (SafeCommand Command)
mismi =
  safeCommand commandP'

commandP' :: Parser Command
commandP' =
  AwsC <$> commandA' <|> AwsK <$> commandK'

commandK' :: Parser AmazonkaCommand
commandK' = subparser $
     command' "uploadk"
              "Upload a file to s3."
              (Uploadk <$> filepath' <*> address')
  <> command' "downloadk"
              "Download a file from s3."
              (Downloadk <$> address' <*> filepath')
  <> command' "sizek"
              "Get the size of an address."
              (Sizek <$> address')
  <> command' "synck"
              "sync between two prefixes."
              (Synck <$> address' <*> address' <*> syncMode' <*> fork')
  <> command' "list"
              "Stream a recursively list of objects on a prefixe"
              (Listk <$> address')
  <> command' "existsk"
              "Check if an address exists."
              (Existsk <$> address')


commandA' :: Parser (AwsCommand)
commandA' = subparser $
     command' "ls"
             "List on a prefix."
              (List <$> address' <*> recursive')
  <> command' "upload"
              "Upload file to s3."
              (Upload <$> filepath' <*> address')
  <> command' "download"
              "Download file from s3."
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
  <> command' "size"
              "Get the size of an address."
              (Size <$> address')
  <> command' "sync"
              "Sync between two prefixes."
              (Sync <$> address' <*> address' <*> syncMode' <*> fork')

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

filepath' :: Parser FilePath
filepath' = strArgument $
     metavar "FILEPATH"
  <> help "Absolute file path, i.e. /tmp/fred"

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
