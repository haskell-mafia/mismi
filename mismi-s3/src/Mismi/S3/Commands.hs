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
  ) where

import           Control.Arrow ((***))

import           Control.Exception (ioError)
import           Control.Lens ((.~), (^.), to, view)
import           Control.Monad.Catch (throwM, onException)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Resource (ResourceT, allocate, runResourceT)
import           Control.Monad.Reader (ask)
import           Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString as BS
import           Data.Conduit (Conduit, Source, ResumableSource)
import           Data.Conduit ((=$=), ($$), ($$+-))
import           Data.Conduit (awaitForever)
import           Data.Conduit.Binary (sinkFile, sinkLbs)
import qualified Data.Conduit.List as DC

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

import           Network.AWS.Data.Body (RqBody (..), RsBody (..), toBody)
import           Network.AWS.Data.Body (ChunkedBody (..), ChunkSize (..))
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
import           System.Directory (createDirectoryIfMissing, doesFileExist)
import           System.FilePath (FilePath, takeDirectory)
import           System.Posix.IO (OpenMode(..), openFd, closeFd, fdSeek, defaultFileFlags)
import qualified "unix-bytestring" System.Posix.IO.ByteString as UBS

import           System.Timeout.Lifted (timeout)
import           System.IO.Error (userError)

import           Twine.Data.Queue (writeQueue)
import           Twine.Parallel (RunError (..), consume)

import           X.Control.Monad.Trans.Either (EitherT, eitherT, left, right, bimapEitherT, runEitherT, newEitherT)

import qualified X.Data.Conduit.Binary as XB

headObject :: Address -> AWS (Maybe HeadObjectResponse)
headObject a =
  handle404 . send . f' A.headObject $ a

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

delete :: Address -> AWS ()
delete =
  void . send . f' A.deleteObject

getObject' :: Address -> AWS (Maybe GetObjectResponse)
getObject' =
  handle404 . send . f' A.getObject

read :: Address -> AWS (Maybe Text)
read a = withRetries 5 $ do
  r <- read' a
  z <- liftIO . sequence $ (runResourceT . ($$+- sinkLbs)) <$> r
  pure $ fmap (T.concat . TL.toChunks . TL.decodeUtf8) z

-- | WARNING : The returned @ResumableResource@ must be comsumed within the
-- @AWS@ monad. Failure to do so can result in run time errors (recv on a bad
-- file descriptor) when the @MonadResouce@ cleans up the socket.
read' :: Address -> AWS (Maybe (ResumableSource (ResourceT IO) BS.ByteString))
read' a = do
  r <- getObject' a
  pure $ fmap (^. A.gorsBody . to _streamBody) r

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
          chunk = 1024 * 1024 * 1024 -- 1 gb
          big = 5 * 1024 * 1024 -- 5 gb
        in
          case s == 0 of
            True ->
              pure Nothing
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
     & A.coServerSideEncryption .~ Just sse & A.coMetadataDirective .~ Just Copy

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

uploadOrFail :: FilePath -> Address -> AWS ()
uploadOrFail f a =
  eitherT hoistUploadError pure $ upload f a

uploadWithModeOrFail :: WriteMode -> FilePath -> Address -> AWS ()
uploadWithModeOrFail w f a =
  eitherT hoistUploadError pure $ uploadWithMode w f a

hoistUploadError :: UploadError -> AWS ()
hoistUploadError e =
  case e of
    UploadSourceMissing f ->
      throwM $ SourceFileMissing f
    UploadDestinationExists a ->
      throwM $ DestinationAlreadyExists a
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
  let chunk = 100 * 1024 * 1024
  case s < chunk of
    True ->
      lift $ uploadSingle f a
    False ->
      case s > 1024 * 1024 * 1024 of
        True ->
          multipartUpload f a s (2 * chunk) 100
        False ->
          multipartUpload f a s chunk 100

uploadSingle :: FilePath -> Address -> AWS ()
uploadSingle file a = do
  rq <- N.chunkedFile (ChunkSize $ 1024 * 1024) file
  void . send $ f' A.putObject a rq & A.poServerSideEncryption .~ pure sse

multipartUpload :: FilePath -> Address -> Integer -> Integer -> Int -> EitherT UploadError AWS ()
multipartUpload file a fileSize chunk fork = do
  e <- ask
  mpu <- lift $ createMultipartUpload a

  let chunks = calculateChunksCapped (fromInteger fileSize) (fromInteger chunk) 4096 -- max 4096 prts returned

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
  withFile file ReadMode $ \h -> do
    req' <- liftIO $ do
      let cs = (1024 * 1024) -- 1 mb
          cl = toInteger c
          b = XB.slurpHandle h (toInteger o) (Just $ toInteger c)
          cb = ChunkedBody cs cl b
      return . f' A.uploadPart a i mpu $ Chunked cb

    r <- runEitherT . runAWS e $ send req'
    case r of
      Left z ->
        pure $! Left z

      Right z -> do
        m <- fromMaybeM (throwM . Invariant $ "uprsETag") $ z ^. A.uprsETag
        pure $! Right $! PartResponse i m


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
    then multipartDownload a f sz 100 100
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
