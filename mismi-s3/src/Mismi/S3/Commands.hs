{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mismi.S3.Commands (
    headObject
  , exists
  , existsPrefix
  , getSize
  , size
  , sizeRecursively
  , sizeRecursively'
  , delete
  , read
  , read'
  , concatMultipart
  , copy
  , copyWithMode
  , copyMultipart
  , move
  , upload
  , uploadOrFail
  , uploadWithMode
  , uploadWithModeOrFail
  , uploadRecursive
  , uploadRecursiveOrFail
  , uploadRecursiveWithMode
  , uploadRecursiveWithModeOrFail
  , multipartUpload
  , uploadSingle
  , write
  , writeOrFail
  , writeWithMode
  , writeWithModeOrFail
  , getObjects
  , getObjectsRecursively
  , listObjects
  , list
  , list'
  , download
  , downloadOrFail
  , downloadWithMode
  , downloadWithModeOrFail
  , downloadSingle
  , downloadWithRange
  , downloadRecursive
  , downloadRecursiveOrFail
  , downloadRecursiveWithMode
  , downloadRecursiveWithModeOrFail
  , multipartDownload
  , listMultipartParts
  , listMultiparts
  , listOldMultiparts
  , listOldMultiparts'
  , abortMultipart
  , abortMultipart'
  , filterOld
  , filterNDays
  , listRecursively
  , listRecursively'
  , sync
  , syncWithMode
  , createMultipartUpload
  , grantReadAccess
  , hoistUploadError
  , hoistDownloadError
  , chunkFilesBySize
  ) where

import           Control.Arrow ((***))
import           Control.Concurrent.Async.Lifted (mapConcurrently_)
import           Control.Exception (ioError)
import qualified Control.Exception as CE
import           Control.Lens ((.~), (^.), to, view)
import           Control.Monad.Catch (Handler(..), throwM, onException)
import           Control.Monad.Extra (concatMapM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader (ask)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Resource (ResourceT, allocate, runResourceT, liftResourceT)
import qualified Control.Retry as Retry

import qualified Data.ByteString as BS
import           Data.Conduit ((=$=), ($$), ($$+-))
import           Data.Conduit (Conduit, Source, ResumableSource)
import           Data.Conduit (awaitForever)
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.Internal as Conduit (ResumableSource(..), ConduitM(..), Pipe(..))
import           Data.Conduit.Binary (sinkFile, sinkLbs)
import qualified Data.Conduit.List as DC
import           Data.IORef (IORef)
import qualified Data.IORef as IORef

import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime, addUTCTime)

import           Mismi.Amazonka (Env, send, paginate)
import           Mismi.Control
import           Mismi.S3.Core.Data
import           Mismi.S3.Data
import           Mismi.S3.Internal
import qualified Mismi.S3.Patch.Network as N
import qualified Mismi.S3.Patch.PutObjectACL as P

import qualified Network.AWS as A
import           Network.AWS.Data.Body (ChunkedBody (..), ChunkSize (..))
import           Network.AWS.Data.Body (RqBody (..), RsBody (..), toBody)
import           Network.AWS.Data.Text (toText)
import           Network.AWS.S3 (BucketName (..))
import           Network.AWS.S3 (GetObjectResponse, HeadObjectResponse)
import           Network.AWS.S3 (ListObjects, ListObjectsResponse)
import           Network.AWS.S3 (MetadataDirective (..))
import           Network.AWS.S3 (MultipartUpload, Part)
import           Network.AWS.S3 (Object, ObjectKey (..))
import qualified Network.AWS.S3 as A

import           P

import           System.IO (IO, IOMode (..), SeekMode (..))
import           System.IO (hFileSize, hSetFileSize, withFile)
import           System.IO.Error (IOError)
import           System.Directory (createDirectoryIfMissing, doesFileExist, getDirectoryContents)
import           System.FilePath (FilePath, (</>), takeDirectory)
import           System.Posix.IO (OpenMode(..), openFd, closeFd, fdSeek, defaultFileFlags)
import           System.Posix.Files (fileSize, getFileStatus, isDirectory, isRegularFile)
import qualified "unix-bytestring" System.Posix.IO.ByteString as UBS

import           System.Timeout.Lifted (timeout)
import           System.IO.Error (userError)

import           Twine.Data.Queue (writeQueue)
import           Twine.Parallel (RunError (..), consume)

import           X.Control.Monad.Trans.Either (EitherT, eitherT, left, right, bimapEitherT, hoistMaybe
                                                , runEitherT, newEitherT)

import qualified X.Data.Conduit.Binary as XB

-- | Retrieves the 'HeadObjectResponse'. Handles any 404 response by converting to Maybe.
--
headObject :: Address -> AWS (Maybe HeadObjectResponse)
headObject a =
  handle404 . send . f' A.headObject $ a

-- | Checks for the existence of 'Address'.
--
exists :: Address -> AWS Bool
exists a =
  headObject a >>= pure . isJust

existsPrefix :: Address -> AWS Bool
existsPrefix (Address (Bucket b) (Key k)) =
  fmap (\r -> length (view A.lorsContents r) == 1 || length (view A.lorsCommonPrefixes r) == 1) . send $ A.listObjects (BucketName b)
    & A.loPrefix .~ Just ((+/) k)
    & A.loDelimiter .~ Just '/'
    & A.loMaxKeys .~ Just 1

getSize :: Address -> AWS (Maybe Int)
getSize a =
  size a >>= pure . fmap fromIntegral
{-# DEPRECATED getSize "Use Mismi.S3.Commands.size instead" #-}

size :: Address -> AWS (Maybe Bytes)
size a =
  headObject a >>= pure . fmap (Bytes . fromIntegral) . maybe Nothing (^. A.horsContentLength)

sizeRecursively :: Address -> AWS [Sized Address]
sizeRecursively prefix =
  sizeRecursively' prefix $$ DC.consume

sizeRecursively' :: Address -> Source AWS (Sized Address)
sizeRecursively' (Address b (Key k)) =
  let
    cmd =
      A.listObjects (BucketName $ unBucket b)
        & A.loPrefix .~ Just k
  in
    paginate cmd =$=
    DC.mapFoldable (takeObjectSizes b)

takeObjectSizes :: Bucket -> ListObjectsResponse -> [Sized Address]
takeObjectSizes b lors =
  with (lors ^. A.lorsContents) $ \o ->
    let
      ObjectKey k =
        o ^. A.oKey

      bytes =
        Bytes $ fromIntegral (o ^. A.oSize)
        -- We shouldn't need this fromIntegral but amazonka incorrectly uses
        -- an Int instead of Int64 for sizes, we don't want to propagate this
        -- mistake.
        --
        -- See https://github.com/brendanhay/amazonka/issues/320
    in
      Sized bytes $ Address b (Key k)

-- | Delete 'Address'
--
delete :: Address -> AWS ()
delete =
  void . send . f' A.deleteObject

-- | Retrieve the object at 'Address'. Handles any 404 response by converting to Maybe.
getObject' :: Address -> AWS (Maybe GetObjectResponse)
getObject' =
  handle404 . send . f' A.getObject

-- | Read contents of 'Address'.
--
read :: Address -> AWS (Maybe Text)
read a = withRetries 5 $ do
  r <- read' a
  z <- liftIO . sequence $ (runResourceT . ($$+- sinkLbs)) <$> r
  pure $ fmap (T.concat . TL.toChunks . TL.decodeUtf8) z

countBytes ::
      IORef Int64
   -> Source (ResourceT IO) BS.ByteString
   -> Source (ResourceT IO) BS.ByteString
countBytes ref src =
  let
    loop = do
      mbs <- Conduit.await
      case mbs of
        Nothing ->
          pure ()
        Just bs -> do
          liftIO $ IORef.modifyIORef' ref (+ fromIntegral (BS.length bs))
          Conduit.yield bs
          loop
  in
    src =$= loop

readRange ::
     Int
  -> Int
  -> Address
  -> AWS (Source (ResourceT IO) BS.ByteString, ResourceT IO ())
readRange start end a = do
  result <-
    send $
      f' A.getObject a
        & A.goRange .~ Just (bytesRange start end)

  liftResourceT . Conduit.unwrapResumable $
    result ^. A.gorsBody . to _streamBody

readRetry ::
     Env
  -> Retry.RetryStatus
  -> IORef (ResourceT IO ())
  -> IORef Int64
  -> Int
  -> Address
  -> Source (ResourceT IO) BS.ByteString
readRetry env status0 finalizerRef startRef end a = do
  start <- fmap fromIntegral . liftIO $ IORef.readIORef startRef

  (source, finalizer) <- A.runAWS env $ readRange start end a
  liftIO $ IORef.writeIORef finalizerRef finalizer

  Conduit.catchC (countBytes startRef source) $ \(err :: CE.SomeException) -> do
    status <- liftIO $ throwOrRetry 5 err status0
    readRetry env status finalizerRef startRef end a

newResumableSource :: Source m a -> m () -> ResumableSource m a
newResumableSource (Conduit.ConduitM source) final =
  Conduit.ResumableSource (source Conduit.Done) final

-- | WARNING : The returned @ResumableResource@ must be consumed within the
-- @AWS@ monad. Failure to do so can result in run time errors (recv on a bad
-- file descriptor) when the @MonadResouce@ cleans up the socket.
read' :: Address -> AWS (Maybe (ResumableSource (ResourceT IO) BS.ByteString))
read' a = do
  env <- ask
  startRef <- liftIO $ IORef.newIORef 0
  mend <- getSize a
  case mend of
    Nothing ->
      pure Nothing

    Just 0 ->
      pure . Just $ newResumableSource mempty (pure ())

    Just end -> do
      finalizerRef <- liftIO $ IORef.newIORef (pure ())

      let
        source =
          readRetry env Retry.defaultRetryStatus finalizerRef startRef end a

        final = do
          finalizer <- liftIO $ IORef.readIORef finalizerRef
          finalizer

      pure . Just $
        newResumableSource source final

concatMultipart :: WriteMode -> Int -> [Address] -> Address -> EitherT ConcatError AWS ()
concatMultipart mode fork inputs dest = do
  when (mode == Fail) .
    whenM (lift $ exists dest) .
      left $ ConcatDestinationExists $ dest

  when (null inputs) $
    left NoInputFiles

  things <- fmap (join . catMaybes) . forM inputs $ \input -> do
    r <- lift $ size input
    case r of
      Nothing ->
        left $ ConcatSourceMissing input
      Just x ->
        let
          s = fromIntegral $ unBytes x
          minChunk = 5 * 1024 * 1024 -- 5 MiB
          chunk = 1024 * 1024 * 1024 -- 1 gb
          big = 5 * 1024 * 1024 -- 5 gb
        in
          case s == 0 of
            True ->
              pure Nothing
            False ->
              case s < minChunk of
                True ->
                  left $ ConcatSourceTooSmall input s
                False ->
                  case s < big of
                    True ->
                      pure $ Just [(input, 0, s)]
                    False ->
                      let
                        chunks = calculateChunksCapped s chunk 4096
                      in
                        pure . Just $ (\(a, b, _) -> (input, a, b)) <$> chunks

  when (null things) $
    left NoInputFilesWithData

  e <- ask
  mpu <- lift $ createMultipartUpload dest

  let
    (is, bs, ls) = L.unzip3 things
    chunks = L.zip4 is bs ls [1..]

  rs <- liftIO $
    consume (forM_ chunks . writeQueue) fork $ multipartCopyWorker e mpu dest

  let
    abort =
      lift $ abortMultipart' dest mpu

  case rs of
    Left f ->
      abort >>
        (left $ ConcatCopyError f)

    Right prts ->
      flip onException abort $
        void . send $ f' A.completeMultipartUpload dest mpu &
          A.cMultipartUpload .~ pure (A.completedMultipartUpload & A.cmuParts .~ sortPartResponse (snd prts))

copy :: Address -> Address -> EitherT CopyError AWS ()
copy s d =
  copyWithMode Overwrite s d

copyWithMode :: WriteMode -> Address -> Address -> EitherT CopyError AWS ()
copyWithMode mode s d = do
  unlessM (lift $ exists s) . left $ CopySourceMissing s
  when (mode == Fail) . whenM (lift $ exists d) . left $ CopyDestinationExists $ d
  sz' <- lift $ getSize s
  sz <- fromMaybeM (left $ CopySourceSize s) sz'
  let
    chunk = 100 * 1024 * 1024 -- 100 mb
    big = 1024 * 1024 * 1024 -- 1 gb
  case sz < big of
    True ->
      lift $ copySingle s d
    False ->
      copyMultipart s d sz chunk 100

copySingle :: Address -> Address -> AWS ()
copySingle (Address (Bucket sb) (Key sk)) (Address (Bucket b) (Key dk)) =
  void . send $ A.copyObject (BucketName b) (sb <> "/" <> sk) (ObjectKey dk)
     & A.coServerSideEncryption .~ Just sse & A.coMetadataDirective .~ Just MDCopy

copyMultipart :: Address -> Address -> Int -> Int -> Int -> EitherT CopyError AWS ()
copyMultipart source dest sz chunk fork = do
  e <- ask
  mpu <- lift $ createMultipartUpload dest -- target

  let
    chunks = calculateChunksCapped sz chunk 4096
    things = (\(o, c, i) -> (source, o, c, i)) <$> chunks

  r <- liftIO $
    consume (forM_ things . writeQueue) fork $ multipartCopyWorker e mpu dest

  let abort =
        lift $ abortMultipart' dest mpu

  case r of
    Left f ->
      abort >>
        (left $ MultipartCopyError f)

    Right prts ->
      flip onException abort $
        void . send $ f' A.completeMultipartUpload dest mpu &
          A.cMultipartUpload .~ pure (A.completedMultipartUpload & A.cmuParts .~ sortPartResponse (snd prts))

-- Sort is required here because the completeMultipartUpload api expects an
-- ascending list of part id's
sortPartResponse :: [PartResponse] -> Maybe (NEL.NonEmpty A.CompletedPart)
sortPartResponse prts =
 let z = sortOn (\(PartResponse i _) -> i) prts
     l = (\(PartResponse i etag) -> A.completedPart i etag) <$> z
 in NEL.nonEmpty l

multipartCopyWorker :: Env -> Text -> Address -> (Address, Int, Int, Int) -> IO (Either Error PartResponse)
multipartCopyWorker e mpu dest (source, o, c, i) = do
  let
    sb = unBucket $ bucket source
    sk = unKey $ key source
    db = unBucket $ bucket dest
    dk = unKey $ key dest
    req =
      A.uploadPartCopy (BucketName db) (sb <> "/" <> sk) (ObjectKey dk) i mpu
        & A.upcCopySourceRange .~ (Just $ bytesRange o (o + c - 1))

  Retry.recovering (Retry.fullJitterBackoff 500000) [s3Condition] $ \_ -> do
    r <- runEitherT . runAWS e $ send req
    case r of
      Left z ->
        pure $! Left z

      Right z -> do
        pr <- fromMaybeM (throwM . Invariant $ "upcrsCopyPartResult") $ z ^. A.upcrsCopyPartResult
        m <- fromMaybeM (throwM . Invariant $ "cprETag") $ pr ^. A.cprETag
        pure $! Right $! PartResponse i m

createMultipartUpload :: Address -> AWS Text
createMultipartUpload a = do
  mpu <- send $ f' A.createMultipartUpload a & A.cmuServerSideEncryption .~ Just sse
  maybe (throwM . Invariant $ "MultipartUpload: missing 'UploadId'") pure (mpu ^. A.cmursUploadId)

move :: Address -> Address -> EitherT CopyError AWS ()
move source destination' =
  copy source destination' >>
    lift (delete source)

upload :: FilePath -> Address -> EitherT UploadError AWS ()
upload =
  uploadWithMode Fail

uploadRecursive :: FilePath -> Address -> Int -> EitherT UploadError AWS ()
uploadRecursive =
  uploadRecursiveWithMode Fail

uploadRecursiveOrFail :: FilePath -> Address -> Int -> AWS ()
uploadRecursiveOrFail f a i =
  eitherT hoistUploadError pure $ uploadRecursive f a i

uploadOrFail :: FilePath -> Address -> AWS ()
uploadOrFail f a =
  eitherT hoistUploadError pure $ upload f a

uploadWithModeOrFail :: WriteMode -> FilePath -> Address -> AWS ()
uploadWithModeOrFail w f a =
  eitherT hoistUploadError pure $ uploadWithMode w f a

uploadRecursiveWithModeOrFail :: WriteMode -> FilePath -> Address -> Int -> AWS ()
uploadRecursiveWithModeOrFail w f a i =
  eitherT hoistUploadError pure $ uploadRecursiveWithMode w f a i

hoistUploadError :: UploadError -> AWS ()
hoistUploadError e =
  case e of
    UploadSourceMissing f ->
      throwM $ SourceFileMissing f
    UploadDestinationExists a ->
      throwM $ DestinationAlreadyExists a
    UploadSourceNotDirectory f ->
      throwM $ SourceNotDirectory f
    MultipartUploadError (WorkerError a) ->
      throwM $ a
    MultipartUploadError (BlowUpError a) ->
      throwM $ a

uploadWithMode :: WriteMode -> FilePath -> Address -> EitherT UploadError AWS ()
uploadWithMode m f a = do
  when (m == Fail) . whenM (lift $ exists a) . left $ UploadDestinationExists a
  unlessM (liftIO $ doesFileExist f) . left $ UploadSourceMissing f
  s <- liftIO $ withFile f ReadMode $ \h ->
    hFileSize h
  case s < bigChunkSize of
    True ->
      lift $ uploadSingle f a
    False ->
      -- Originally had a concurrency of 100 (instead of 20).
      --
      -- Based on the reasoning behind downloadWithMode which resulted in a 5
      -- as it's concurrency default. Testing showed that for upload 20 was a
      -- better default.
      case s > 1024 * 1024 * 1024 of
        True ->
          multipartUpload f a s (2 * bigChunkSize) 20
        False ->
          multipartUpload f a s bigChunkSize 20



bigChunkSize :: Integer
bigChunkSize = 100 * 1024 * 1024


uploadSingle :: FilePath -> Address -> AWS ()
uploadSingle file a = do
  rq <- N.chunkedFile (ChunkSize $ 1024 * 1024) file
  void . send $ f' A.putObject a rq & A.poServerSideEncryption .~ pure sse

multipartUpload :: FilePath -> Address -> Integer -> Integer -> Int -> EitherT UploadError AWS ()
multipartUpload file a fSize chunk fork = do
  e <- ask
  mpu <- lift $ createMultipartUpload a

  let chunks = calculateChunksCapped (fromInteger fSize) (fromInteger chunk) 4096 -- max 4096 prts returned

  r <- liftIO $
    consume (forM_ chunks . writeQueue) fork $ multipartUploadWorker e mpu file a

  let abort = lift $ abortMultipart' a mpu

  case r of
    Left f ->
      abort >>
        (left $ MultipartUploadError f)

    Right prts ->
      flip onException abort $
        void . send $ f' A.completeMultipartUpload a mpu &
          A.cMultipartUpload .~ pure (A.completedMultipartUpload & A.cmuParts .~ sortPartResponse (snd prts))


multipartUploadWorker :: Env -> Text -> FilePath -> Address -> (Int, Int, Int) -> IO (Either Error PartResponse)
multipartUploadWorker e mpu file a (o, c, i) =
  withFile file ReadMode $ \h ->
    let
      cs = (1024 * 1024) -- 1 mb
      cl = toInteger c
      b = XB.slurpHandle h (toInteger o) (Just $ toInteger c)
      cb = ChunkedBody cs cl b
      req' = f' A.uploadPart a i mpu $ Chunked cb
    in
    Retry.recovering (Retry.fullJitterBackoff 500000) [s3Condition] $ \_ -> do
      r <- runEitherT . runAWS e $ send req'
      case r of
        Left z ->
          pure $! Left z
        Right z -> do
          m <- fromMaybeM (throwM MissingETag) $ z ^. A.uprsETag
          pure $! Right $! PartResponse i m

s3Condition :: Applicative a => Retry.RetryStatus -> Handler a Bool
s3Condition s =
  Handler $ \(ex :: S3Error) ->
    pure $ case ex of
      MissingETag ->
        Retry.rsIterNumber s < 5
      _ ->
        False

uploadRecursiveWithMode :: WriteMode -> FilePath -> Address -> Int -> EitherT UploadError AWS ()
uploadRecursiveWithMode mode src (Address buck ky) fork = do
  es <- tryIO $ getFileStatus src
  case es of
    Left _ -> left $ UploadSourceMissing src
    Right st -> unless (isDirectory st) . left $ UploadSourceNotDirectory src
  files <- liftIO (listRecursivelyLocal src)
  mapM_ uploadFiles $ chunkFilesBySize fork (fromIntegral bigChunkSize) files
  where
    uploadFiles :: [(FilePath, Int64)] -> EitherT UploadError AWS ()
    uploadFiles [] = pure ()
    uploadFiles [(f,s)]
      | fromIntegral s < bigChunkSize = lift . uploadSingle f $ uploadAddress f
      | otherwise = uploadWithMode mode f $ uploadAddress f
    uploadFiles xs =
      mapConcurrently_ (\ (f, _) -> lift . uploadSingle f $ uploadAddress f) xs


    prefixLen = L.length (src </> "a") - 1

    uploadAddress :: FilePath -> Address
    uploadAddress fp = Address buck (ky // Key (T.pack $ L.drop prefixLen fp))

-- Take a list of files and their sizes, and convert it to a list of tests
-- where the total size of the files in the sub list is less than `maxSize`
-- and the length of the sub lists is <= `maxCount`.
chunkFilesBySize :: Int -> Int64 -> [(FilePath, Int64)] -> [[(FilePath, Int64)]]
chunkFilesBySize maxCount maxSize =
  takeFiles 0 [] . L.sortOn snd
  where
    takeFiles :: Int64 -> [(FilePath, Int64)] -> [(FilePath, Int64)] -> [[(FilePath, Int64)]]
    takeFiles _ acc [] = [acc]
    takeFiles current acc ((x, s):xs) =
      if current + s < maxSize && L.length acc < maxCount
        then takeFiles (current + s) ((x, s):acc) xs
        else acc : takeFiles s [(x, s)] xs

-- | Like `listRecursively` but for the local filesystem.
-- Also returns
listRecursivelyLocal :: MonadIO m => FilePath -> m [(FilePath, Int64)]
listRecursivelyLocal topdir = do
  entries <- liftIO $ listDirectory topdir
  (dirs, files) <- liftIO . partitionDirsFilesWithSizes $ fmap (topdir </>) entries
  others <- concatMapM listRecursivelyLocal dirs
  pure $ files <> others


-- Not available with ghc 7.10 so copy it here.
listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  filter f <$> getDirectoryContents path
  where
    f filename =
      filename /= "." && filename /= ".."

partitionDirsFilesWithSizes :: MonadIO m => [FilePath] -> m ([FilePath], [(FilePath, Int64)])
partitionDirsFilesWithSizes =
  pworker ([], [])
  where
    pworker (dirs, files) [] = pure (dirs, files)
    pworker (dirs, files) (x:xs) = do
      xstat <- liftIO $ getFileStatus x
      let xsize = fromIntegral $ fileSize xstat
          newDirs = if isDirectory xstat then x : dirs else dirs
          newFiles = if isRegularFile xstat then (x, xsize) : files else files
      pworker (newDirs, newFiles) xs

write :: Address -> Text -> AWS WriteResult
write =
  writeWithMode Fail

writeOrFail :: Address -> Text -> AWS ()
writeOrFail a t =
  write a t >>= liftWriteResult

writeWithModeOrFail :: WriteMode -> Address -> Text -> AWS ()
writeWithModeOrFail m a t =
  writeWithMode m a t >>= liftWriteResult

liftWriteResult :: WriteResult -> AWS ()
liftWriteResult = \case
  WriteOk ->
    pure ()
  WriteDestinationExists a ->
    throwM $ DestinationAlreadyExists a

writeWithMode :: WriteMode -> Address -> Text -> AWS WriteResult
writeWithMode w a t = eitherT pure (const $ pure WriteOk) $ do
  case w of
    Fail -> whenM (lift $ exists a) . left $ WriteDestinationExists a
    Overwrite -> return ()
  void . lift . send $ f' A.putObject a (toBody . T.encodeUtf8 $ t) & A.poServerSideEncryption .~ Just sse

-- pair of prefixs and keys
getObjects :: Address -> AWS ([Key], [Key])
getObjects (Address (Bucket buck) (Key ky)) =
  ((Key <$>) *** ((\(ObjectKey t) -> Key t) <$>)) <$> ff (A.listObjects (BucketName buck) & A.loPrefix .~ Just ((+/) ky) & A.loDelimiter .~ Just '/' )
  where
    ff :: ListObjects -> AWS ([T.Text], [ObjectKey])
    ff b = do
      r <- send b
      if r ^. A.lorsIsTruncated == Just True
        then
        do
          let d = (maybeToList =<< fmap (^. A.cpPrefix) (r ^. A.lorsCommonPrefixes), fmap (^. A.oKey) (r ^. A.lorsContents))
          n <- ff $ b & A.loMarker .~ (r ^. A.lorsNextMarker)
          pure $ d <> n
        else
        pure (maybeToList =<< fmap (^. A.cpPrefix) (r ^. A.lorsCommonPrefixes), fmap (^. A.oKey) (r ^. A.lorsContents))

getObjectsRecursively :: Address -> AWS [Object]
getObjectsRecursively (Address (Bucket b) (Key ky)) =
  getObjects' $ A.listObjects (BucketName b) & A.loPrefix .~ Just ((+/) ky)
  where
    -- Hoping this will have ok performance in cases where the results are large, it shouldnt
    -- affect correctness since we search through the list for it anyway
    go x ks = (NEL.toList ks <>) <$> getObjects' (x & A.loMarker .~ Just (toText $ NEL.last ks ^. A.oKey))
    getObjects' :: ListObjects -> AWS [Object]
    getObjects' x = do
      resp <- send x
      if resp ^. A.lorsIsTruncated == Just True
        then
          maybe
            (throwM . Invariant $ "Truncated response with empty contents list.")
            (go x)
            (NEL.nonEmpty $ resp ^. A.lorsContents)
        else
          pure $ resp ^. A.lorsContents

-- | Return a tuple of the prefixes and keys at the provided S3 Address.
listObjects :: Address -> AWS ([Address], [Address])
listObjects a =
  (\(p, k) -> (Address (bucket a) <$> p, Address (bucket a) <$> k)) <$> getObjects a

list :: Address -> AWS [Address]
list a =
  list' a $$ DC.consume

list' :: Address -> Source AWS Address
list' a@(Address (Bucket b) (Key k)) =
  let run s = s =$= liftAddressAndPrefix a
  in run . paginate $
    A.listObjects (BucketName b)
      & A.loPrefix .~ Just ((+/) k)
      & A.loDelimiter .~ Just '/'

liftAddressAndPrefix :: Address -> Conduit ListObjectsResponse AWS Address
liftAddressAndPrefix a =
  DC.mapFoldable (\r ->
       fmap (\o ->
         let ObjectKey t = o ^. A.oKey
         in a { key = Key t }) (r ^. A.lorsContents)
    <> join (forM (r ^. A.lorsCommonPrefixes) $ \cp ->
         maybeToList . fmap (\cp' -> a { key = Key cp' }) $ cp ^. A.cpPrefix))

-- | add a "/" at the end of some text if missing and if the text is not empty
(+/) :: Text -> Text
(+/) k
  | T.null k           = ""
  | T.isSuffixOf "/" k = k
  | otherwise          = k <> "/"

hoistDownloadError :: DownloadError -> AWS ()
hoistDownloadError e =
  case e of
    DownloadSourceMissing a ->
      throwM $ SourceMissing DownloadError a
    DownloadDestinationExists f ->
      throwM $ DestinationFileExists f
    DownloadDestinationNotDirectory f ->
      throwM $ DestinationNotDirectory f
    DownloadInvariant a b ->
      throwM $ Invariant (renderDownloadError $ DownloadInvariant a b)
    MultipartError (WorkerError a) ->
      throwM a
    MultipartError (BlowUpError a) ->
      throwM a

download :: Address -> FilePath -> EitherT DownloadError AWS ()
download =
  downloadWithMode Fail

downloadOrFail :: Address -> FilePath -> AWS ()
downloadOrFail a f =
  eitherT hoistDownloadError pure $ download a f

downloadWithMode :: WriteMode -> Address -> FilePath -> EitherT DownloadError AWS ()
downloadWithMode mode a f = do
  when (mode == Fail) . whenM (liftIO $ doesFileExist f) . left $ DownloadDestinationExists f
  liftIO $ createDirectoryIfMissing True (takeDirectory f)

  sz' <- lift $ getSize a
  sz <- maybe (left $ DownloadSourceMissing a) right sz'

  if (sz > 200 * 1024 * 1024)
    then -- Originally had a concurrecy of 100 (instead of 5). Tested a number of
         -- values between 2 and 100 and found empirically that 5 gave the fastest
         -- downloads (less than 10% better), but significantly reduced the
         -- likelihood of triggering the S3 rate limiter (by a factor of 20)
         -- which in turn reduces the liklehood of `IOExceptions` and hung
         -- threads.
         multipartDownload a f sz 100 5
    else downloadSingle a f

downloadWithModeOrFail :: WriteMode -> Address -> FilePath -> AWS ()
downloadWithModeOrFail m a f =
  eitherT hoistDownloadError pure $ downloadWithMode m a f

downloadSingle :: Address -> FilePath -> EitherT DownloadError AWS ()
downloadSingle a f = do
  r <- (lift $ getObject' a) >>= maybe (left $ DownloadSourceMissing a) right
  liftIO . withRetries 5 . withFileSafe f $ \p ->
    runResourceT . ($$+- sinkFile p) $ r ^. A.gorsBody ^. to _streamBody

multipartDownload :: Address -> FilePath -> Int -> Integer -> Int -> EitherT DownloadError AWS ()
multipartDownload source destination sz chunk fork = bimapEitherT MultipartError id $ do
  e <- ask
  let chunks = calculateChunks sz (fromInteger $ chunk * 1024 * 1024)
  void . withFileSafe destination $ \f -> do
    liftIO $ withFile f WriteMode $ \h ->
      hSetFileSize h (toInteger sz)

    newEitherT . liftIO .
      consume (\q -> mapM (writeQueue q) chunks) fork $ \(o, c, _) ->
        runEitherT . runAWS e $ downloadWithRange source o (o + c) f

downloadWithRange :: Address -> Int -> Int -> FilePath -> AWS ()
downloadWithRange a start end dest = withRetries 5 $ do
  -- Use a timeout of ten minutes. Arrivied at empirically. With a timeout of 5
  -- minutes this was triggering too often. Want this to be the last resort.
  res <- timeout (10 * 60 * 1000 * 1000) $ do
          r <- send $ f' A.getObject a &
            A.goRange .~ (Just $ bytesRange start end)

          -- write to file
          liftIO . runResourceT $ do
            fd <- snd <$> allocate (openFd dest WriteOnly Nothing defaultFileFlags) closeFd
            void . liftIO $ fdSeek fd AbsoluteSeek (fromInteger . toInteger $ start)
            let source = r ^. A.gorsBody ^. to _streamBody
            let sink = awaitForever $ liftIO . UBS.fdWrite fd
            source $$+- sink

  case res of
    Just () -> pure ()
    Nothing -> liftIO $ ioError (userError "downloadWithRange timeout")

downloadRecursiveWithMode :: WriteMode -> Address -> FilePath -> EitherT DownloadError AWS ()
downloadRecursiveWithMode mode src dest = do
  -- Check if the destination already exists and is not a directory.
  es <- tryIO $ getFileStatus dest
  case es of
    Left _ -> pure ()
    Right st -> unless (isDirectory st) . left $ DownloadDestinationNotDirectory dest
  -- Real business starts here.
  addrs <- lift $ listRecursively src
  mapM_ drWorker addrs
  where
    drWorker :: Address -> EitherT DownloadError AWS ()
    drWorker addr = do
      fpdest <- hoistMaybe (DownloadInvariant addr src) $
                    ((</>) dest) . T.unpack . unKey <$> removeCommonPrefix src addr
      downloadWithMode mode addr fpdest

downloadRecursive :: Address -> FilePath -> EitherT DownloadError AWS ()
downloadRecursive =
  downloadRecursiveWithMode Fail

downloadRecursiveOrFail :: Address -> FilePath -> AWS ()
downloadRecursiveOrFail a f =
  eitherT hoistDownloadError pure $ downloadRecursive a f

downloadRecursiveWithModeOrFail :: WriteMode -> Address -> FilePath -> AWS ()
downloadRecursiveWithModeOrFail m a f =
  eitherT hoistDownloadError pure $ downloadRecursiveWithMode m a f

listMultipartParts :: Address -> Text -> AWS [Part]
listMultipartParts a uploadId = do
  let req = f' A.listParts a uploadId
  paginate req $$ DC.foldMap (^. A.lprsParts)

listMultiparts :: Bucket -> AWS [MultipartUpload]
listMultiparts (Bucket bn) = do
  let req = A.listMultipartUploads $ BucketName bn
  paginate req $$ DC.foldMap (^. A.lmursUploads)

listOldMultiparts :: Bucket -> AWS [MultipartUpload]
listOldMultiparts b = do
  mus <- listMultiparts b
  now <- liftIO getCurrentTime
  pure $ filter (filterOld now) mus

listOldMultiparts' :: Bucket -> Int -> AWS [MultipartUpload]
listOldMultiparts' b i = do
  mus <- listMultiparts b
  now <- liftIO getCurrentTime
  pure $ filter (filterNDays i now) mus

filterOld :: UTCTime -> MultipartUpload -> Bool
filterOld = filterNDays 7

filterNDays :: Int -> UTCTime -> MultipartUpload -> Bool
filterNDays n now m = case m ^. A.muInitiated of
  Nothing -> False
  Just x -> nDaysOld n now x

nDaysOld :: Int -> UTCTime -> UTCTime -> Bool
nDaysOld n now utc = do
  let n' = fromInteger $ toInteger n
  let diff = -1 * 60 * 60 * 24 * n' :: NominalDiffTime
  let boundary = addUTCTime diff now
  boundary > utc

abortMultipart :: Bucket -> MultipartUpload -> AWS ()
abortMultipart (Bucket b) mu = do
  (ObjectKey k) <- maybe (throwM $ Invariant "Multipart key missing") pure (mu ^. A.muKey)
  i <- maybe (throwM $ Invariant "Multipart uploadId missing") pure (mu ^. A.muUploadId)
  abortMultipart' (Address (Bucket b) (Key k)) i

abortMultipart' :: Address -> Text -> AWS ()
abortMultipart' a i =
  void . send $ f' A.abortMultipartUpload a i

listRecursively :: Address -> AWS [Address]
listRecursively a =
  listRecursively' a $$ DC.consume

listRecursively' :: Address -> Source AWS Address
listRecursively' a@(Address (Bucket bn) (Key k)) =
  paginate (A.listObjects (BucketName bn) & A.loPrefix .~ Just k) =$= liftAddress a

liftAddress :: Address -> Conduit ListObjectsResponse AWS Address
liftAddress a =
  DC.mapFoldable (\r -> (\o -> a { key = Key (let ObjectKey t = o ^. A.oKey in t) }) <$> (r ^. A.lorsContents) )

grantReadAccess :: Address -> ReadGrant -> AWS ()
grantReadAccess a g =
  void . send $ (f' P.putObjectACL a & P.poaGrantRead .~ Just (readGrant g))

sync :: Address -> Address -> Int -> EitherT SyncError AWS ()
sync =
  syncWithMode FailSync

syncWithMode :: SyncMode -> Address -> Address -> Int -> EitherT SyncError AWS ()
syncWithMode mode source dest fork = do
  e <- ask
  bimapEitherT SyncError id . void . newEitherT . liftIO $
    (consume (sinkQueue e (listRecursively' source)) fork (worker source dest mode e))

worker :: Address -> Address -> SyncMode -> Env -> Address -> IO (Either SyncWorkerError ())
worker input output mode env f = runEitherT . runAWST env SyncAws $ do
  n <- maybe (left $ SyncInvariant input f) right $ removeCommonPrefix input f
  let out = withKey (// n) output
      liftCopy = bimapEitherT SyncCopyError id
      cp = liftCopy $ copy f out
  foldSyncMode
    (ifM (lift $ exists out) (left $ OutputExists out) cp)
    (liftCopy $ copyWithMode Overwrite f out)
    (ifM (lift $ exists out) (right ()) cp)
    mode

tryIO :: MonadIO m => IO a -> m (Either IOError a)
tryIO = liftIO . CE.try
