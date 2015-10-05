{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import           BuildInfo_ambiata_mismi_s3

import           Data.Conduit
import qualified Data.Conduit.List as DC

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Resource

import qualified Data.ByteString as BS
import           Data.Text.IO (putStrLn)
import           Data.Text hiding (copy)

import           Mismi.Environment
import           Mismi.S3

import           Options.Applicative

import           P

import           System.IO hiding (putStrLn)
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
    Upload FilePath Address
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
      RunCommand DryRun c ->
        print c >> exitSuccess
      RunCommand RealRun c ->
        run c

run :: Command -> IO ()
run c = do
  e <- orDie envErrorRender . EitherT $ discoverAWSEnv
  orDie errorRender . runAWS e $ case c of
    Upload s d ->
      uploadOrFail s d
    Download s d ->
      download s d
    Copy s d ->
      copy s d
    Move s d ->
      move s d
    Exists a ->
      exists a >>= \b -> liftIO $ unless b exitFailure
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
      syncWithMode m s d f
    List a rq ->
      rec (list' a) (listRecursively' a) rq $$ DC.mapM_ (liftIO . putStrLn . addressToText)


mismi :: Parser (SafeCommand Command)
mismi = safeCommand commandP'

commandP' :: Parser Command
commandP' = subparser $
     command' "upload"
              "Upload a file to s3."
              (Upload <$> filepath' <*> address')
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
