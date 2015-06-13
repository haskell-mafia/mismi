{-# LANGUAGE NoImplicitPrelude #-}

import           BuildInfo_mismi_s3

import           Control.Monad.IO.Class

import           Data.Text hiding (copy)

import           Mismi.S3

import           Options.Applicative

import           P

import           System.IO
import           System.Exit

import           X.Options.Applicative

data RunType =
  DryRun | RealRun
  deriving (Eq, Show)

data SafeCommand =
    Version
  | RunCommand RunType Command
  deriving (Eq, Show)

data Recursive =
  Recursive
  | NotRecursive
  deriving (Eq, Show)

rec :: a -> a -> Recursive -> a
rec notrecursive recursive r = case r of
  NotRecursive -> notrecursive
  Recursive -> recursive

data Command =
  List Address Recursive
  | Upload FilePath Address
  | Download Address FilePath
  | Copy Address Address
  | Move Address Address
  | Exists Address
  | Delete Address
  | Write Address Text WriteMode
  | Read Address
  deriving (Eq, Show)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  dispatch mismi >>= \sc ->
    case sc of
      Version ->
        putStrLn ("mismi: " <> buildInfoVersion) >> exitSuccess
      RunCommand DryRun c ->
        print c >> exitSuccess
      RunCommand RealRun c ->
        run c

run :: Command -> IO ()
run c = runS3WithDefaults $ case c of
  List a r ->
    rec (list a) (listRecursively a) r >>= liftIO . mapM_ (putStrLn . unpack . addressToText)
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

mismi :: Parser SafeCommand
mismi =
  Version <$ flag' () (short 'v' <> long "version" <> help "Display the version information for the eagle executable.")
  <|> (RunCommand <$> flag RealRun DryRun (long "dry-run" <> hidden) <*> commandP')

commandP' :: Parser Command
commandP' = subparser $
     command' "ls"
             "List on a prefix."
              (List <$> address' <*> recursive')
  <> command' "upload"
              "Upload file to s3."
              (Upload <$> filepath' <*> address')
  <> command' "download"
              "Downlaod file from s3."
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
