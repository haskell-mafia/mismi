{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import           BuildInfo_ambiata_mismi_cli
import           DependencyInfo_ambiata_mismi_cli

import           Control.Lens (over, set)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Resource (runResourceT)

import qualified Data.ByteString as BS
import           Data.Conduit ((=$=), ($$), ($$+-))
import qualified Data.Conduit.List as DC
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.String (String)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Mismi.Amazonka (serviceRetry, retryAttempts, exponentBase, configure)
import qualified Mismi.Environment as C
import qualified Mismi.OpenSSL as O
import           Mismi.S3
import           Mismi.S3.Amazonka (s3)

import           P hiding (All)

import           System.IO (BufferMode (..), FilePath, IO, hSetBuffering, print, stderr, stdout)
import           System.Environment (lookupEnv)
import           System.Exit (ExitCode (..), exitFailure, exitSuccess)
import qualified System.FilePath as FP
import           System.Posix.Signals (Handler (..), installHandler, sigINT, sigQUIT, sigTERM)
import           System.Posix.Process (exitImmediately)

import           Text.Printf (printf)

import           X.Control.Monad.Trans.Either.Exit (orDie)
import           X.Control.Monad.Trans.Either (EitherT, eitherT, runEitherT, firstEitherT)
import           X.Options.Applicative (Completer, Parser, RunType (..), SafeCommand (..)
                                       , action, auto, command', flag, flag', help, long, metavar
                                       , pOption, option, short, value)
import qualified X.Options.Applicative as XOA

data Recursive =
    NotRecursive
  | Recursive
    deriving (Eq, Show)

rec :: a -> a -> Recursive -> a
rec notrecursive recursive r =
  case r of
    NotRecursive ->
      notrecursive
    Recursive ->
      recursive

data Detail =
    All
  | Summary
    deriving (Eq, Show)

data UnitPrefix =
    NoPrefix
  | IEC
  | Metric
    deriving (Eq, Show)

data Command =
    Upload Recursive FilePath Address WriteMode Int
  | Download Recursive Address FilePath
  | Copy Address Address WriteMode
  | Concat [Address] Address WriteMode Int
  | Move Address Address
  | Exists Address
  | Delete Address Recursive
  | Write Address Text WriteMode
  | Read Address
  | Cat Address
  | Size Address Recursive Detail UnitPrefix
  | Sync Address Address SyncMode Int
  | List Address Recursive
  deriving (Eq, Show)

data Force =
    NotForce
  | Force
    deriving (Eq, Show)

parseForce :: Maybe String -> Force
parseForce f =
  case f of
    Just "true" ->
      Force
    _ ->
      NotForce

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  forM_ [sigINT, sigTERM, sigQUIT] $ \s ->
    installHandler s (Catch . void . exitImmediately $ ExitFailure 111) Nothing

  f <- lookupEnv "AWS_FORCE"
  XOA.dispatch (mismi $ parseForce f) >>= \case
      VersionCommand ->
        T.putStrLn ("s3: " <> T.pack buildInfoVersion) >> exitSuccess
      DependencyCommand ->
        mapM (T.putStrLn . T.pack)  dependencyInfo >> exitSuccess
      RunCommand DryRun c ->
        print c >> exitSuccess
      RunCommand RealRun c ->
        run c

run :: Command -> IO ()
run c = do
  openssl <- fmap (maybe False (\s -> s == "true" || s == "1")) $ lookupEnv "AWS_OPENSSL"

  e <- case openssl of
    True ->
      orDie O.renderRegionError O.discoverAWSEnv
    False ->
      orDie C.renderRegionError C.discoverAWSEnv
  let
    e' = configure (over serviceRetry (set retryAttempts 10 . set exponentBase 0.6) s3) e
  orDie O.renderError . O.runAWS e' $ case c of
    Upload NotRecursive s d m _ ->
      uploadWithModeOrFail m s d
    Upload Recursive s d m f ->
      uploadRecursiveWithModeOrFail m s d f
    Download NotRecursive s d ->
      renderExit renderDownloadError . download s . optAppendFileName d $ key s
    Download Recursive s d ->
      renderExit renderDownloadError $ downloadRecursive s d
    Copy s d m ->
      renderExit renderCopyError $ copyWithMode m s d
    Concat ss d m f ->
      renderExit renderConcatError $ concatMultipart m f ss d
    Move s d ->
      renderExit renderCopyError $ move s d
    Exists a ->
      exists a >>= liftIO . flip unless exitFailure
    Delete a r ->
      rec (delete a) (listRecursively' a $$ DC.mapM_ delete) r
    Write a t w ->
      writeWithModeOrFail w a t
    Read a -> do
      liftIO $ T.hPutStrLn stderr "Warning: `s3 read` is deprecated. Use `s3 cat` instead."
      read a >>= \md -> liftIO $ maybe exitFailure pure md >>= T.putStrLn
    Cat a ->
      read' a >>= \md -> liftIO $ maybe exitFailure pure md >>= runResourceT . ($$+- DC.mapM_ (liftIO . BS.putStr))
    Size a NotRecursive _ u ->
      size a >>= liftIO . maybe exitFailure (T.putStrLn . renderSize u)
    Size a Recursive d u ->
      sizeRecursive a d u
    Sync s d m f ->
      renderExit renderSyncError $ syncWithMode m s d f
    List a rq ->
      rec (list' a) (listRecursively' a) rq $$ DC.mapM_ (liftIO . T.putStrLn . addressToText)

sizeRecursive :: Address -> Detail -> UnitPrefix -> AWS ()
sizeRecursive root d p =
  case d of
    All ->
      sizeRecursively' root $$ DC.mapM_ (liftIO . T.putStrLn . renderSizedAddress p)
    Summary -> do
      bytes <- sizeRecursively' root $$ DC.map sizedBytes =$= DC.fold (+) 0
      liftIO . T.putStrLn $ renderSizedAddress p (Sized bytes root)


renderSizedAddress :: UnitPrefix -> Sized Address -> Text
renderSizedAddress p (Sized bytes address) =
  let
    separator =
      case p of
        NoPrefix ->
          "\t"
        IEC ->
          " "
        Metric ->
          " "
  in
    renderSize p bytes <> separator <> addressToText address

renderSize :: UnitPrefix -> Bytes -> Text
renderSize p bytes =
  case p of
    NoPrefix ->
      T.pack (show $ unBytes bytes)
    IEC ->
      renderUnit iec bytes
    Metric ->
      renderUnit metric bytes

iec :: Scale Bytes
iec =
  mkScale "B" [
    -- The threshold here is not obvious. The cutoff is lower than you might
    -- expect. This is so that we only ever render a maximum of 3 characters
    -- for a value: e.g. 1000-1023 MiB are rendered as 1.0 GiB
      Suffix (1000 * pow 1024 0) (pow 1024 1) "KiB"
    , Suffix (1000 * pow 1024 1) (pow 1024 2) "MiB"
    , Suffix (1000 * pow 1024 2) (pow 1024 3) "GiB"
    , Suffix (1000 * pow 1024 3) (pow 1024 4) "TiB"
    , Suffix (1000 * pow 1024 4) (pow 1024 5) "PiB"
    , Suffix (1000 * pow 1024 5) (pow 1024 6) "EiB"
    ]

metric :: Scale Bytes
metric =
  mkScale "B" [
      Suffix (pow 1000 1) (pow 1000 1) "kB"
    , Suffix (pow 1000 2) (pow 1000 2) "MB"
    , Suffix (pow 1000 3) (pow 1000 3) "GB"
    , Suffix (pow 1000 4) (pow 1000 4) "TB"
    , Suffix (pow 1000 5) (pow 1000 5) "PB"
    , Suffix (pow 1000 6) (pow 1000 6) "EB"
    ]

pow :: Bytes -> Int64 -> Bytes
pow b p =
  b ^ p

data Scale a =
  Scale {
      scaleOne :: Suffix a
    , scaleTable :: Map a (Suffix a)
    } deriving (Show)

data Suffix a =
  Suffix {
      suffixThreshold :: a
    , suffixMagnitude :: a
    , suffixText :: Text
    } deriving (Show)

mapText :: (Text -> Text) -> Suffix a -> Suffix a
mapText f (Suffix t m x) =
  Suffix t m (f x)

mkScale :: (Ord a, Num a) => Text -> [Suffix a] -> Scale a
mkScale one0 table0 =
  let
    len =
      List.maximum .
      fmap T.length $
      one0 : fmap suffixText table0

    one =
      T.justifyLeft len ' ' one0

    table =
      fmap (\s -> (suffixThreshold s, s)) $
      fmap (mapText $ T.justifyLeft len ' ') table0
  in
    Scale (Suffix 1 1 one) $
    Map.fromList table

lookupSuffix :: Ord a => a -> Scale a -> Suffix a
lookupSuffix n scale =
  case Map.splitLookup n $ scaleTable scale of
    (xs, Nothing, _) ->
      fromMaybe
        (scaleOne scale)
        (fmap fst $ Map.maxView xs)
    (_, Just x, _) ->
      x

renderUnit :: Scale Bytes -> Bytes -> Text
renderUnit scale n0 =
  let
    suffix =
      lookupSuffix n0 scale

    n :: Double
    n =
      fromIntegral n0 / fromIntegral (suffixMagnitude suffix)
  in
    if suffixMagnitude suffix == 1 || n >= 10 then
      T.pack (printf "%3.0f" n) <> " " <> suffixText suffix
    else
      T.pack (printf "%3.1f" n) <> " " <> suffixText suffix

renderExit :: MonadIO m => (e -> Text) -> EitherT e m a -> m a
renderExit f =
  eitherT (\e -> liftIO $ (T.hPutStrLn stderr $ f e) >> exitFailure) return

optAppendFileName :: FilePath -> Key -> FilePath
optAppendFileName f k = fromMaybe f $ do
  fp <- valueOrEmpty (FP.hasTrailingPathSeparator f || FP.takeFileName f == ".") (FP.takeDirectory f)
  bn <- basename k
  pure . FP.combine fp $ T.unpack bn

mismi :: Force -> Parser (SafeCommand Command)
mismi f =
  XOA.safeCommand (commandP' f <|> deprecatedCommandP')

commandP' :: Force -> Parser Command
commandP' f = XOA.subparser $
     command' "upload"
              "Upload a file to s3."
              (Upload <$> recursive' <*> filepath' <*> address' <*> writeMode' f <*> fork')
  <> command' "download"
              "Download a file from s3."
              (Download <$> recursive' <*> address' <*> filepath')
  <> command' "copy"
              "Copy a file from an S3 address to another S3 address."
              (Copy <$> address' <*> address' <*> writeMode' f)
  <> command' "concat"
              "Concatenate many files together into one S3 address."
              (Concat <$> some address' <*> outputAddress' <*> writeMode' f <*> fork')
  <> command' "move"
              "Move an S3 address to another S3 address"
              (Move <$> address' <*> address')
  <> command' "exists"
              "Check if an address exists."
              (Exists <$> address')
  <> command' "delete"
              "Delete an address."
              (Delete <$> address' <*> recursive')
  <> command' "write"
              "Write to an address."
              (Write <$> address' <*> text' <*> writeMode' f)
  <> command' "cat"
              "Read raw data from an address and write it to stdout."
              (Cat <$> address')
  <> command' "size"
              "Get the size of an address."
              (Size <$> address' <*> recursive' <*> detail' <*> prefix')
  <> command' "sync"
              "Sync between two prefixes."
              (Sync <$> address' <*> address' <*> syncMode' <*> fork')
  <> command' "ls"
              "Stream a recursively list of objects on a prefix."
              (List <$> address' <*> recursive')

deprecatedCommandP' :: Parser Command
deprecatedCommandP' = XOA.subparser $
  XOA.internal <> command'
    "read"
    "Read from an address. (Deprecated in favour of `s3 cat`.)"
    (Read <$> address')


recursive' :: Parser Recursive
recursive' =
  flag NotRecursive Recursive $
       short 'r'
    <> long "recursive"
    <> help "Recursively list"

prefix' :: Parser UnitPrefix
prefix' =
  iec' <|> metric' <|> pure NoPrefix

iec' :: Parser UnitPrefix
iec' =
  flag' IEC $
       short 'h'
    <> long "iec"
    <> help "Human-readable output, using IEC unit prefixes, i.e. 1024^n"

metric' :: Parser UnitPrefix
metric' =
  flag' Metric $
       short 'm'
    <> long "metric"
    <> help "Human-readable output, using metric unit prefixes, i.e. 1000^n"

detail' :: Parser Detail
detail' =
  flag All Summary $
       short 's'
    <> long "summary"
    <> help "Display only the combined total for a prefix."

address' :: Parser Address
address' = XOA.argument (pOption s3Parser) $
     metavar "S3URI"
  <> help "An s3 uri, i.e. s3://bucket/prefix/key"
  <> XOA.completer addressCompleter

outputAddress' :: Parser Address
outputAddress' = option (pOption s3Parser) $
     long "output"
  <> metavar "S3URI"
  <> help "An s3 uri, i.e. s3://bucket/prefix/key"
  <> XOA.completer addressCompleter

filepath' :: Parser FilePath
filepath' = XOA.strArgument $
     metavar "FILEPATH"
  <> help "Absolute file path, i.e. /tmp/fred"
  <> action "file"

text' :: Parser Text
text' = T.pack <$> (XOA.strArgument $
     metavar "STRING"
  <> help "Data to write to S3 address.")

writeMode' :: Force -> Parser WriteMode
writeMode' ff = do
  flip fmap (flag Fail Overwrite $ long "overwrite" <> help "WriteMode") $ \f ->
    case f of
      Overwrite ->
        Overwrite
      Fail ->
        case ff of
          Force ->
            Overwrite
          NotForce ->
            Fail

syncMode' :: Parser SyncMode
syncMode' =
      pure FailSync
  <|> (flag' SkipSync $ long "skip" <> help "Skip over files that already exist in the target location.")
  <|> (flag' OverwriteSync $ long "overwrite" <> help "Overwrite files that already exist in the target location.")

fork' :: Parser Int
fork' = XOA.option auto $
     long "fork"
  <> metavar "INT"
  <> help "Number of threads to fork CopyObject call by."
  <> value 8

addressCompleter :: Completer
addressCompleter = XOA.mkCompleter $ \s -> do
  res <- runEitherT (go s)
  case res of
    Left   _ -> pure []
    Right xs -> pure xs
  where
    go :: String -> EitherT () IO [String]
    go s = do
      e <- forget O.discoverAWSEnv
      forget $ O.runAWS e $ do
        x <- case addressFromText (T.pack s) of
          Nothing ->
            pure []
          Just a ->
            let a' = withKey dirname a
            in  fmap (T.unpack . addressToText) <$> list a'
        let x' = filter (isPrefixOf s) x
        pure x'
    forget = firstEitherT (const ())
