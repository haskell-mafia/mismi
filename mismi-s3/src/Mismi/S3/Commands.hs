{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mismi.S3.Commands (
    module Mismi.Control
  , DownloadError (..)
  , renderDownloadError
  , headObject
  , exists
  , getSize
  , delete
  , read
  , read'
  , copy
  , copyWithMode
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
  , downloadWithMode
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
  , SyncError (..)
  , SyncWorkerError (..)
  , renderSyncError
  , grantReadAccess
  , sse
  ) where


import           Control.Arrow ((***))

import           Control.Lens ((&), (.~), (^.), to)
import           Control.Monad.Catch (throwM, onException)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Control.Monad.Reader (ask)
import           Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString as BS
import           Data.Conduit (Conduit, Source, ResumableSource)
import           Data.Conduit ((=$=), ($$), ($$+-))
import           Data.Conduit (awaitForever)
import           Data.Conduit.Binary (sinkFile, sinkLbs)
import qualified Data.Conduit.List as DC

import qualified Data.List.NonEmpty as NEL
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime, addUTCTime)

import           Mismi.Control
import           Mismi.S3.Amazonka (Env, send, paginate)
import           Mismi.S3.Data
import           Mismi.S3.Internal
import qualified Mismi.S3.Patch.Network as N
import qualified Mismi.S3.Patch.PutObjectACL as P

import           Network.AWS.Data.Body (RqBody (..), RsBody (..), toBody)
import           Network.AWS.Data.Body (ChunkedBody (..), ChunkSize (..))
import           Network.AWS.Data.Text (toText)
import           Network.AWS.S3 (BucketName (..))
import           Network.AWS.S3 (ETag)
import           Network.AWS.S3 (GetObjectResponse, gorsBody)
import           Network.AWS.S3 (HeadObjectResponse, horsContentLength)
import           Network.AWS.S3 (ListObjects, loPrefix, loDelimiter, loMarker)
import           Network.AWS.S3 (ListObjectsResponse, lorsContents, lorsCommonPrefixes)
import           Network.AWS.S3 (lorsIsTruncated, lorsNextMarker)
import           Network.AWS.S3 (MetadataDirective (..))
import           Network.AWS.S3 (MultipartUpload, muKey, muUploadId, muInitiated)
import           Network.AWS.S3 (Object, oKey)
import           Network.AWS.S3 (ObjectKey (..))
import           Network.AWS.S3 (Part)
import           Network.AWS.S3 (ServerSideEncryption (..))
import           Network.AWS.S3 (cMultipartUpload)
import           Network.AWS.S3 (cmuParts, cmuServerSideEncryption)
import           Network.AWS.S3 (cmursUploadId)
import           Network.AWS.S3 (coMetadataDirective, coServerSideEncryption)
import           Network.AWS.S3 (cpPrefix)
import           Network.AWS.S3 (goRange)
import           Network.AWS.S3 (lmursUploads)
import           Network.AWS.S3 (lprsParts)
import           Network.AWS.S3 (poServerSideEncryption)
import           Network.AWS.S3 (uprsETag)
import           Network.AWS.S3 (createMultipartUpload, abortMultipartUpload, listMultipartUploads)
import           Network.AWS.S3 (completeMultipartUpload, completedMultipartUpload, completedPart)
import           Network.AWS.S3 (getObject, putObject, copyObject, deleteObject)
import qualified Network.AWS.S3 as A

import           P
import           Prelude (($!))

import           System.IO (IO, IOMode (..), SeekMode (..))
import           System.IO (hFileSize, hSetFileSize, withFile)
import           System.Directory (createDirectoryIfMissing, doesFileExist)
import           System.FilePath (FilePath, takeDirectory)
import           System.Posix.IO (OpenMode(..), openFd, closeFd, fdSeek, defaultFileFlags)
import qualified "unix-bytestring" System.Posix.IO.ByteString as UBS

import           Twine.Data.Queue (writeQueue)
import           Twine.Parallel (RunError (..), renderRunError, consume)

import           X.Control.Monad.Trans.Either (EitherT, eitherT, left, right, bimapEitherT, runEitherT, newEitherT)

import qualified X.Data.Conduit.Binary as XB

headObject :: Address -> AWS (Maybe HeadObjectResponse)
headObject a =
  handle404 . send . fencode' A.headObject $ a

exists :: Address -> AWS Bool
exists a =
  headObject a >>= pure . isJust

getSize :: Address -> AWS (Maybe Int)
getSize a =
  headObject a >>= pure . maybe Nothing (^. horsContentLength)

delete :: Address -> AWS ()
delete =
  void . send . fencode' deleteObject

getObject' :: Address -> AWS (Maybe GetObjectResponse)
getObject' =
  handle404 . send . fencode' getObject

read :: Address -> AWS (Maybe Text)
read a = do
  r <- read' a
  z <- liftIO . sequence $ (runResourceT . ($$+- sinkLbs)) <$> r
  pure $ fmap (T.concat . TL.toChunks . TL.decodeUtf8) z

read' :: Address -> AWS (Maybe (ResumableSource (ResourceT IO) BS.ByteString))
read' a = do
  r <- getObject' a
  pure $ fmap (^. gorsBody . to _streamBody) r

copy :: Address -> Address -> AWS ()
copy =
  copyWithMode Fail

copyWithMode :: WriteMode -> Address -> Address -> AWS ()
copyWithMode mode s d = do
  unlessM (exists s) . throwM $ SourceMissing CopyError s
  foldWriteMode  (whenM (exists d) . throwM . DestinationAlreadyExists $ d) (pure ()) mode
  copy' s d

copy' :: Address -> Address -> AWS ()
copy' (Address (Bucket sb) (Key sk)) (Address (Bucket b) (Key dk)) =
  void . send $ copyObject (BucketName b) (sb <> "/" <> sk) (ObjectKey dk)
     & coServerSideEncryption .~ Just sse & coMetadataDirective .~ Just Copy

move :: Address -> Address -> AWS ()
move source destination' =
  copy source destination' >>
    delete source

upload :: FilePath -> Address -> EitherT UploadError AWS ()
upload =
  uploadWithMode Fail

uploadOrFail :: FilePath -> Address -> AWS ()
uploadOrFail f a =
  eitherT liftUploadError pure $ upload f a

uploadWithModeOrFail :: WriteMode -> FilePath -> Address -> AWS ()
uploadWithModeOrFail w f a =
  eitherT liftUploadError pure $ uploadWithMode w f a

liftUploadError :: UploadError -> AWS ()
liftUploadError e =
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
  void . send $ fencode' putObject a rq & poServerSideEncryption .~ pure sse

data PartResponse =
  PartResponse !Int !ETag

multipartUpload :: FilePath -> Address -> Integer -> Integer -> Int -> EitherT UploadError AWS ()
multipartUpload file a fileSize chunk fork = do
  e <- ask
  mpu' <- send $ fencode' createMultipartUpload a & cmuServerSideEncryption .~ pure sse
  mpu <- maybe (throwM . Invariant $ "MultipartUpload: missing 'UploadId'") pure (mpu' ^. cmursUploadId)

  let chunks = calculateChunksCapped (fromInteger fileSize) (fromInteger chunk) 4096 -- max 4096 prts returned

  r <- liftIO $
    consume (forM_ chunks . writeQueue) fork $ multipartUploadWorker e mpu file a

  let abort =
        void . send . fencode' abortMultipartUpload a $ mpu

  case r of
    Left f ->
      abort >>
        (left $ MultipartUploadError f)

    Right prts -> do
      -- Sort is required here because the completeMultipartUpload api expects an
      -- ascending list of part id's
      let z = sortOn (\(PartResponse i _) -> i) $ snd prts
          l = (\(PartResponse i etag) -> completedPart i etag) <$> z
          ncps = NEL.nonEmpty l

      flip onException abort $
        void . send $ fencode' completeMultipartUpload a mpu &
          cMultipartUpload .~ pure (completedMultipartUpload & cmuParts .~ ncps)


multipartUploadWorker :: Env -> Text -> FilePath -> Address -> (Int, Int, Int) -> IO (Either Error PartResponse)
multipartUploadWorker e mpu file a (o, c, i) =
  withFile file ReadMode $ \h -> do
    req' <- liftIO $ do
      let cs = (1024 * 1024) -- 1 mb
          cl = toInteger c
          b = XB.slurpHandle h (toInteger o) (Just $ toInteger c)
          cb = ChunkedBody cs cl b
      return . fencode' A.uploadPart a i mpu $ Chunked cb

    r <- runEitherT . runAWS e $ send req'
    case r of
      Left z ->
        pure $! Left z

      Right z -> do
        m <- fromMaybeM (throwM . Invariant $ "uprsETag") $ z ^. uprsETag
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
  void . lift . send $ fencode' putObject a (toBody . T.encodeUtf8 $ t) & poServerSideEncryption .~ Just sse

-- pair of prefixs and keys
getObjects :: Address -> AWS ([Key], [Key])
getObjects (Address (Bucket buck) (Key ky)) =
  ((Key <$>) *** ((\(ObjectKey t) -> Key t) <$>)) <$> ff (A.listObjects (BucketName buck) & loPrefix .~ Just ((+/) ky) & loDelimiter .~ Just '/' )
  where
    ff :: ListObjects -> AWS ([T.Text], [ObjectKey])
    ff b = do
      r <- send b
      if r ^. lorsIsTruncated == Just True
        then
        do
          let d = (maybeToList =<< fmap (^. cpPrefix) (r ^. lorsCommonPrefixes), fmap (^. oKey) (r ^. lorsContents))
          n <- ff $ b & loMarker .~ (r ^. lorsNextMarker)
          pure $ d <> n
        else
        pure (maybeToList =<< fmap (^. cpPrefix) (r ^. lorsCommonPrefixes), fmap (^. oKey) (r ^. lorsContents))

getObjectsRecursively :: Address -> AWS [Object]
getObjectsRecursively (Address (Bucket b) (Key ky)) =
  getObjects' $ A.listObjects (BucketName b) & loPrefix .~ Just ((+/) ky)
  where
    -- Hoping this will have ok performance in cases where the results are large, it shouldnt
    -- affect correctness since we search through the list for it anyway
    go x ks = (NEL.toList ks <>) <$> getObjects' (x & loMarker .~ Just (toText $ NEL.last ks ^. oKey))
    getObjects' :: ListObjects -> AWS [Object]
    getObjects' x = do
      resp <- send x
      if resp ^. lorsIsTruncated == Just True
        then
          maybe
            (throwM . Invariant $ "Truncated response with empty contents list.")
            (go x)
            (NEL.nonEmpty $ resp ^. lorsContents)
        else
          pure $ resp ^. lorsContents

-- Pair of list of prefixes and list of keys
listObjects :: Address -> AWS ([Address], [Address])
listObjects a =
  (\(p, k) -> (Address (bucket a) <$> p, Address (bucket a) <$> k)) <$> getObjects a

list :: Address -> AWS [Address]
list a =
  list' a $$ DC.consume

list' :: Address -> Source AWS Address
list' a@(Address (Bucket b) (Key k)) =
  paginate (A.listObjects (BucketName b) & loPrefix .~ Just ((+/) k) & loDelimiter .~ Just '/') =$= liftAddressAndPrefix a

liftAddressAndPrefix :: Address -> Conduit ListObjectsResponse AWS Address
liftAddressAndPrefix a =
  DC.mapFoldable (\r ->
       fmap (\o -> let ObjectKey t = o ^. oKey in a { key = Key t })(r ^. lorsContents)
    <> join (traverse (\cp -> maybeToList .fmap (\cp' -> a { key = Key cp' }) $ cp ^. cpPrefix) (r ^. lorsCommonPrefixes))
  )

-- | add a "/" at the end of some text if missing and if the text is not empty
(+/) :: Text -> Text
(+/) k
  | T.null k           = ""
  | T.isSuffixOf "/" k = k
  | otherwise          = k <> "/"


data DownloadError =
    DownloadSourceMissing Address
  | DownloadDestinationExists FilePath
  | MultipartError (RunError Error)

renderDownloadError :: DownloadError -> Text
renderDownloadError d =
  case d of
    DownloadSourceMissing a ->
      "Can not download when the source object does not exist [" <> addressToText a <> "]"
    DownloadDestinationExists f ->
      "Can not download to a target that already exists [" <> T.pack f <> "]"
    MultipartError r ->
      "Multipart download error: " <> renderRunError r renderError

download :: Address -> FilePath -> EitherT DownloadError AWS ()
download =
  downloadWithMode Fail

downloadWithMode :: WriteMode -> Address -> FilePath -> EitherT DownloadError AWS ()
downloadWithMode mode a f = do
  when (mode == Fail) . whenM (liftIO $ doesFileExist f) . left $ DownloadDestinationExists f
  liftIO $ createDirectoryIfMissing True (takeDirectory f)

  size' <- lift $ getSize a
  size <- maybe (left $ DownloadSourceMissing a) right size'

  if (size > 200 * 1024 * 1024)
    then multipartDownload a f size 100 100
    else downloadSingle a f

downloadSingle :: Address -> FilePath -> EitherT DownloadError AWS ()
downloadSingle a f = do
  r <- (lift $ getObject' a) >>= maybe (left $ DownloadSourceMissing a) right
  liftIO . withFileSafe f $ \p ->
    runResourceT . ($$+- sinkFile p) $ r ^. gorsBody ^. to _streamBody

multipartDownload :: Address -> FilePath -> Int -> Integer -> Int -> EitherT DownloadError AWS ()
multipartDownload source destination size chunk fork = bimapEitherT MultipartError id $ do
  e <- ask
  let chunks = calculateChunks size (fromInteger $ chunk * 1024 * 1024)
  void . withFileSafe destination $ \f -> do
    liftIO $ withFile f WriteMode $ \h ->
      hSetFileSize h (toInteger size)

    newEitherT . liftIO .
      consume (\q -> mapM (writeQueue q) chunks) fork $ \(o, c, _) ->
        runEitherT . runAWS e $ downloadWithRange source o (o + c) f

downloadWithRange :: Address -> Int -> Int -> FilePath -> AWS ()
downloadWithRange a start end dest = do
  r <- send $ fencode' getObject a &
    goRange .~ (Just $ downRange start end)

  -- write to file
  liftIO $ do
    fd <- openFd dest WriteOnly Nothing defaultFileFlags
    void $ fdSeek fd AbsoluteSeek (fromInteger . toInteger $ start)
    let source = r ^. A.gorsBody ^. to _streamBody
    let sink = awaitForever $ liftIO . UBS.fdWrite fd
    runResourceT $ source $$+- sink
    closeFd fd


listMultipartParts :: Address -> T.Text -> AWS [Part]
listMultipartParts a uploadId = do
  let req = fencode' A.listParts a uploadId
  paginate req $$ DC.foldMap (^. lprsParts)

listMultiparts :: Bucket -> AWS [MultipartUpload]
listMultiparts (Bucket bn) = do
  let req = listMultipartUploads $ BucketName bn
  paginate req $$ DC.foldMap (^. lmursUploads)

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
filterNDays n now m = case m ^. muInitiated of
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
  (ObjectKey k) <- maybe (throwM $ Invariant "Multipart key missing") pure (mu ^. muKey)
  i <- maybe (throwM $ Invariant "Multipart uploadId missing") pure (mu ^. muUploadId)
  abortMultipart' (Address (Bucket b) (Key k)) i

abortMultipart' :: Address -> T.Text -> AWS ()
abortMultipart' a i =
  void . send $ fencode' abortMultipartUpload a i

listRecursively :: Address -> AWS [Address]
listRecursively a =
  listRecursively' a $$ DC.consume

listRecursively' :: Address -> Source AWS Address
listRecursively' a@(Address (Bucket bn) (Key k)) =
  paginate (A.listObjects (BucketName bn) & loPrefix .~ Just k) =$= liftAddress a

liftAddress :: Address -> Conduit ListObjectsResponse AWS Address
liftAddress a =
  DC.mapFoldable (\r -> (\o -> a { key = Key (let ObjectKey t = o ^. oKey in t) }) <$> (r ^. lorsContents) )

grantReadAccess :: Address -> ReadGrant -> AWS ()
grantReadAccess a g =
  void . send $ (fencode' P.putObjectACL a & P.poaGrantRead .~ Just (readGrant g))

sync :: Address -> Address -> Int -> EitherT SyncError AWS ()
sync =
  syncWithMode FailSync

syncWithMode :: SyncMode -> Address -> Address -> Int -> EitherT SyncError AWS ()
syncWithMode mode source dest fork = do
  e <- ask
  bimapEitherT SyncError id . void . newEitherT . liftIO $
    (consume (sinkQueue e (listRecursively' source)) fork (worker source dest mode e))



newtype SyncError =
  SyncError (RunError SyncWorkerError) deriving Show

renderSyncError :: SyncError -> Text
renderSyncError (SyncError r) =
  renderRunError r renderSyncWorkerError

data SyncWorkerError =
   SyncInvariant Address Address
 | OutputExists Address
 | SyncAws Error
 deriving Show

renderSyncWorkerError :: SyncWorkerError -> Text
renderSyncWorkerError w =
  case w of
    SyncInvariant a b ->
      "Remove common prefix invariant: " <>
      "[" <> addressToText b <> "] is not a common prefix of " <>
      "[" <> addressToText a <> "]"
    OutputExists a ->
      "Can not copy to an address that already exists [" <> addressToText a <> "]"
    SyncAws e ->
      "AWS failure during 'sync': " <> renderError e

worker :: Address -> Address -> SyncMode -> Env -> Address -> IO (Either SyncWorkerError ())
worker input output mode env f = runEitherT . runAWST env SyncAws $ do
  n <- maybe (left $ SyncInvariant input f) right $ removeCommonPrefix input f
  let out = withKey (</> n) output
      cp = lift $ copy f out
  foldSyncMode
    (ifM (lift $ exists out) (left $ OutputExists out) cp)
    (lift $ copyWithMode Overwrite f out)
    (ifM (lift $ exists out) (right ()) cp)
    mode

sse :: ServerSideEncryption
sse =
  AES256
