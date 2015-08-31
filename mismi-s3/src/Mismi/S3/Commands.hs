{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
module Mismi.S3.Commands (
    module AWS
  , module A
  , A.AWSError
  , A.throwAWSError
  , headObject
  , exists
  , getSize
  , delete
  , read
  , copy
  , copyWithMode
  , move
  , upload
  , uploadWithMode
  , multipartUpload'
  , uploadSingle
  , write
  , writeWithMode
  , getObjects
  , getObjectsRecursively
  , listObjects
  , list
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
  , retryAWSAction
  , retryAWS
  , retryAWS'
  , sse
  ) where


import           Control.Arrow ((***))

import           Control.Concurrent
import           Control.Concurrent.MSem
import           Control.Concurrent.Async

import           Control.Lens
import           Control.Retry
import           Control.Monad.Catch
import           Control.Monad.Cont (ContT(..))
import           Control.Monad.Morph (hoist)
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Cont (evalContT)
import           Control.Monad.Trans.Either hiding (hoistEither)
import           Control.Monad.Trans.Resource
import           Control.Monad.Reader (ask, local)
import           Control.Monad.IO.Class

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import           Data.Conduit
import qualified Data.Conduit.List as DC
import           Data.Conduit.Binary

import           Data.String
import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL
import qualified Data.Semigroup as SG
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Time.Clock

import           Mismi.Control.Amazonka hiding (AWSError, throwAWSError)
import qualified Mismi.Control.Amazonka as A
import           Mismi.S3.Data
import           Mismi.S3.Internal

import           Network.AWS.S3 hiding (headObject, Bucket, bucket, listObjects)
import qualified Network.AWS.S3 as AWS
import           Network.AWS.Data hiding (Object, list)
import           Network.HTTP.Client (HttpException (..))
import           Network.HTTP.Types.Status (status500, status404)

import           P
import           Prelude (error)

import           System.IO
import           System.IO.Error (userError)
import           System.Directory
import           System.FilePath hiding ((</>))
import           System.Posix.IO
import qualified "unix-bytestring" System.Posix.IO.ByteString as UBS

headObject :: Address -> AWS (Maybe HeadObjectResponse)
headObject a = do
  res <- sendCatch $ fencode' AWS.headObject a
  handle404 res

exists :: Address -> AWS Bool
exists a =
  headObject a >>= pure . maybe False (const True)

getSize :: Address -> AWST IO (Maybe Int)
getSize a =
  headObject a >>= pure . maybe Nothing (^. horContentLength)

delete :: Address -> AWS ()
delete =
  send_ . fencode' deleteObject

handle404 :: Either (ServiceError RESTError) a -> AWS (Maybe a)
handle404 res = case res of
  Left e -> case e of
    HttpError e' -> case e' of
      StatusCodeException s _ _ ->
        if s == status404
          then pure Nothing
          else throwAWSError e
      _ -> throwAWSError e
    SerializerError _ _ -> throwAWSError e
    ServiceError _ s _ ->
      if s == status404
        then pure Nothing
        else throwAWSError e
    Errors _ -> throwAWSError e
  Right rs -> pure $ Just rs

getObject' :: Address -> AWS (Maybe GetObjectResponse)
getObject' a = do
  let req = fencode' getObject a
  res <- sendCatch req
  handle404 res

read :: Address -> AWS (Maybe Text)
read a = do
  resp <- getObject' a
  let format y = T.concat . TL.toChunks . TL.decodeUtf8 $ y
  z <- liftIO . sequence $ (\r -> runResourceT (r ^. gorBody . _RsBody $$+- sinkLbs)) <$> resp
  pure (format <$> z)

copy :: Address -> Address -> AWS ()
copy source dest =
  copyWithMode Fail source dest

copyWithMode :: WriteMode -> Address -> Address -> AWS ()
copyWithMode mode s d = do
  unlessM (exists s) . fail $ "Can not copy when the soruce does not exists exists [" <> (T.unpack $ addressToText s)  <> "]."
  foldWriteMode  (whenM (exists d) . fail $ "Can not copy to a file that already exists [" <> (T.unpack $ addressToText d) <> "].") (pure ()) mode
  copy' s d

copy' :: Address -> Address -> AWS ()
copy' (Address (Bucket sb) (Key sk)) (Address (Bucket b) k) =
  send_ $ copyObject b (sb <> "/" <> sk) (encodeKey k) & coServerSideEncryption .~ Just sse & coMetadataDirective .~ Just Copy

move :: Address -> Address -> AWS ()
move source destination' =
  copy source destination' >>
    delete source

upload :: FilePath -> Address -> AWS ()
upload =
  uploadWithMode Fail

uploadWithMode :: WriteMode -> FilePath -> Address -> AWS ()
uploadWithMode m f a = do
  when (m == Fail) . whenM (exists a) .
    fail $ "Can not upload to a target that already exists [" <> (T.unpack $ addressToText a) <> "]."
  unlessM (liftIO $ doesFileExist f) .
    fail $ "Can not upload when the source does not exist [" <> f <> "]."
  s <- liftIO $ withFile f ReadMode $ \h ->
    hFileSize h
  let chunk = 100 * 1024 * 1024
  if s < chunk
    then do
      uploadSingle f a
    else do
      if (s > 1024 * 1024 * 1024)
         then multipartUpload' f a s (10 * chunk)
         else multipartUpload' f a s chunk

uploadSingle :: FilePath -> Address -> AWS ()
uploadSingle file a = do
  x <- liftIO $ LBS.readFile file
  send_ $ fencode' (putObject $ toBody x) a & poServerSideEncryption .~ pure sse

multipartUpload' :: FilePath -> Address -> Integer -> Integer -> AWS ()
multipartUpload' file a fileSize chunk = do
  e <- ask
  mpu' <- send $ fencode' createMultipartUpload a & cmuServerSideEncryption .~ pure sse
  mpu <- maybe (fail "Failed to create multipart upload") pure (mpu' ^. cmurUploadId)

  let chunks = calculateChunks (fromInteger fileSize) (fromInteger chunk)
  let uploader :: (Int, Int, Int) -> IO (Either A.AWSError UploadPartResponse)
      uploader (o, c, i) = evalContT $ do
         h <- ContT $ withFile file ReadMode
         req' <- liftIO $ do
           hSeek h AbsoluteSeek (toInteger o)
           cont <- LBS.hGetContents h
           let bod = toBody (LBS.take (fromIntegral c) cont)
           return $ fencode' (uploadPart bod) a i mpu
         liftIO . runEitherT . runAWSWithEnv e $ send req'

  prts <- liftIO $ mapConcurrently uploader chunks
  case partitionEithers prts of
    ([], ps) -> do
      let cps = L.zipWith (\(_, _, i) p -> completedPart & cpPartNumber .~ pure i & cpETag .~ (p ^. uprETag)) chunks ps
      send_ $ fencode' completeMultipartUpload a mpu &
          cmu1MultipartUpload .~ (pure $ completedMultipartUpload & cmuParts .~ cps)
    (_, _) -> send_ $ fencode' abortMultipartUpload a mpu

write :: Address -> Text -> AWS ()
write =
  writeWithMode Fail

writeWithMode :: WriteMode -> Address -> Text -> AWS ()
writeWithMode w a t = do
  case w of
    Fail        -> whenM (exists a) . fail $ "Can not write to a file that already exists [" <> show a <> "]."
    Overwrite   -> return ()
  send_ $ fencode' (putObject . toBody . T.encodeUtf8 $ t) a & poServerSideEncryption .~ Just sse

-- pair of prefixs and keys
getObjects :: Address -> AWS ([Key], [Key])
getObjects (Address (Bucket buck) (Key ky)) =
  ((Key <$>) *** (Key <$>)) <$> (ff $ (AWS.listObjects buck & loPrefix .~ Just (pp ky) & loDelimiter .~ Just "/" ))
  where
    pp :: Text -> Text
    pp k = if T.null k then "" else if T.isSuffixOf "/" k then k else k <> "/"
    ff :: ListObjects -> AWS ([T.Text], [T.Text])
    ff b = do
      r <- send b
      if r ^. lorIsTruncated == Just True
        then
        do
          let d = (maybeToList =<< fmap (^. cpPrefix) (r ^. lorCommonPrefixes), fmap (^. oKey) (r ^. lorContents))
          n <- ff $ b & loMarker .~ (r ^. lorNextMarker)
          pure $ (d <> n)
        else
        pure $ (maybeToList =<< fmap (^. cpPrefix) (r ^. lorCommonPrefixes), fmap (^. oKey) (r ^. lorContents))

getObjectsRecursively :: Address -> AWS [Object]
getObjectsRecursively (Address (Bucket b) (Key ky)) =
  getObjects' $ (AWS.listObjects b) & loPrefix .~ Just (pp ky)
  where
    pp :: Text -> Text
    pp k = if T.null k then "" else if T.isSuffixOf "/" k then k else k <> "/"
    -- Hoping this will have ok performance in cases where the results are large, it shouldnt
    -- affect correctness since we search through the list for it anyway
    go x ks = (NEL.toList ks <>) <$> getObjects' (x & loMarker .~ Just (NEL.last ks ^. oKey))
    getObjects' :: ListObjects -> AWS [Object]
    getObjects' x = do
      resp <- send x
      if resp ^. lorIsTruncated == Just True
        then
          maybe
            (Prelude.error "vee: error: truncated response with empty contents list.")
            (go x)
            (NEL.nonEmpty $ resp ^. lorContents)
        else
          pure $ resp ^. lorContents

-- Pair of list of prefixes and list of keys
listObjects :: Address -> AWS ([Address], [Address])
listObjects a =
  (\(p, k) -> (Address (bucket a) <$> p, Address (bucket a) <$> k) )<$> getObjects a

-- list the addresses, keys first, then prefixes
list :: Address -> AWS [Address]
list a =
  (\(p, k) -> k <> p) <$> listObjects a

download :: Address -> FilePath -> AWS ()
download = downloadWithMode Fail

downloadWithMode :: WriteMode -> Address -> FilePath -> AWS ()
downloadWithMode mode a f = do
  when (mode == Fail) . whenM (liftIO $ doesFileExist f) . fail $ "Can not download to a target that already exists [" <> f <> "]."
  unlessM (exists a) . fail $ "Can not download when the source does not exist [" <> (T.unpack $ addressToText a) <> "]."
  liftIO $ createDirectoryIfMissing True (dropFileName f)
  r <- getObject' a
  r' <- maybe (fail "shit") pure r
  liftIO . runResourceT . ($$+- sinkFile f) $ r' ^. gorBody ^. _RsBody

downloadSingle :: Address -> FilePath -> AWS ()
downloadSingle a p = do
  r <- send $ fencode' getObject a
  liftIO . withFileSafe p $ \p' ->
    runResourceT . ($$+- sinkFile p') $ r ^. gorBody ^. _RsBody


multipartDownload :: Address -> FilePath -> Int -> Integer -> Int -> AWS ()
multipartDownload source destination' size chunk' fork = do
  e <- ask

  let chunk = chunk' * 1024 * 1024
  let chunks = calculateChunks size (fromInteger chunk)

  let writer :: (Int, Int, Int) -> IO (Either A.AWSError ())
      writer (o, c, _) = do
        let req = downloadWithRange source o (o + c) destination'
            ioq = runAWSWithEnv e req
        runEitherT ioq

  -- create sparse file
  liftIO $ withFile destination' WriteMode $ \h ->
    hSetFileSize h (toInteger size)

  sem <- liftIO $ new fork
  z <- liftIO $ (mapConcurrently (with sem . writer) chunks)
  hoistAWSError $ foldl' (SG.<>) (Right ()) z

downloadWithRange :: Address -> Int -> Int -> FilePath -> AWS ()
downloadWithRange source start end dest = do
  let req = fencode' getObject source & goRange .~ (Just $ downRange start end)
  r <- send req
  let p :: AWS.GetObjectResponse = r
  let y :: RsBody = p ^. AWS.gorBody

  fd <- liftIO $ openFd dest WriteOnly Nothing defaultFileFlags
  void . liftIO $ fdSeek fd AbsoluteSeek (fromInteger . toInteger $ start)
  liftIO $ do
    let rs :: ResumableSource (ResourceT IO) BS.ByteString = y ^. _RsBody
    let s = awaitForever $ \bs -> liftIO $
              UBS.fdWrite fd bs
    runResourceT $ ($$+- s) rs
  liftIO $ closeFd fd

listMultipartParts :: Address -> T.Text -> AWS [Part]
listMultipartParts a uploadId = do
  let req = fencode' AWS.listParts a uploadId
  paginate req $$ DC.foldMap (^. lprParts)

listMultiparts :: Bucket -> AWS [MultipartUpload]
listMultiparts b = do
  let req = listMultipartUploads $ unBucket b
  paginate req $$ DC.foldMap (^. lmurUploads)

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
  let x :: String -> ServiceError String = ServiceError "amu" status500
  k <- maybe (throwAWSError $ x "Multipart key missing") pure (mu ^. muKey)
  i <- maybe (throwAWSError $ x "Multipart uploadId missing") pure (mu ^. muUploadId)
  abortMultipart' (Address (Bucket b) (Key k)) i

abortMultipart' :: Address -> T.Text -> AWS ()
abortMultipart' a i =
  send_ $ fencode' abortMultipartUpload a i

listRecursively :: Address -> AWS [Address]
listRecursively a = do
  a' <- listRecursively' a
  a' $$ DC.consume

listRecursively' :: Address -> AWS (Source AWS Address)
listRecursively' a@(Address (Bucket b) (Key k)) = do
  e <- ask
  pure . hoist (retryConduit e) $ (paginate $ AWS.listObjects b & loPrefix .~ Just k) =$= liftAddress a

liftAddress :: Address -> Conduit ListObjectsResponse AWS Address
liftAddress a =
  DC.mapFoldable (\r -> (\o -> a { key = Key $ o ^. oKey }) <$> (r ^. lorContents) )

retryConduit :: Env -> AWS a -> AWS a
retryConduit e action =
  local (const e) action

sync :: Address -> Address -> Int -> AWS ()
sync =
  syncWithMode FailSync

syncWithMode :: SyncMode -> Address -> Address -> Int -> AWS ()
syncWithMode mode source dest fork = do
  (c, r) <- liftIO $ (,) <$> newChan <*> newChan
  e <- ask

  -- worker
  tid <- liftIO $ forM [1..fork] (const . forkIO $ worker source dest mode e c r)

  -- sink list to channel
  l <- listRecursively' source
  i <- sinkChanWithDelay 50000 l c

  -- wait for threads and lift errors
  r' <- liftIO $ waitForNResults i r
  liftIO $ forM_ tid killThread
  forM_ r' hoistWorkerResult

hoistWorkerResult :: WorkerResult -> AWS ()
hoistWorkerResult =
  foldWR hoistErr (pure ())

hoistErr :: Err -> AWS ()
hoistErr =
  foldErr throwM (throwM . userError . T.unpack) liftAWSError

worker :: Address -> Address -> SyncMode -> Env -> Chan Address -> Chan WorkerResult -> IO ()
worker source dest mode e c errs = forever $ do
  let invariant = pure . WorkerErr $ Invariant "removeCommonPrefix"
      keep :: Address -> Key -> IO WorkerResult
      keep a k = do
        e' <- runEitherT $ keep' a k
        pure $ case e' of
          Left er -> WorkerErr er
          Right _ -> WorkerOk

      keep' :: Address -> Key -> EitherT Err IO ()
      keep' a k = do
        let out = withKey (</> k) dest
            action :: EitherT Err AWS ()
            action = EitherT $ do
              let cp = copy a out >>= pure . Right
                  ex = exists out
                  te = pure . Left $ Target a out
              foldSyncMode
                (ifM ex te cp)
                cp
                (ifM ex (pure $ Right ()) cp)
                mode
        (mapEitherT (runEitherT . bimapEitherT AwsErr id . runAWSWithEnv e) action >>= EitherT . pure)
          `catchAll` (left . UnknownErr)

  a <- readChan c
  wr <- maybe invariant (keep a) $ removeCommonPrefix source a
  writeChan errs wr

data WorkerResult =
  WorkerOk
  | WorkerErr Err

foldWR :: (Err -> m a) -> m a -> WorkerResult -> m a
foldWR e a = \case
  WorkerOk -> a
  WorkerErr err -> e err

data Err =
  Invariant Text
  | Target Address Address
  | AwsErr A.AWSError
  | UnknownErr SomeException

foldErr :: (SomeException -> m a) -> (Text -> m a) -> (A.AWSError -> m a) -> Err -> m a
foldErr se p err = \case
  Invariant t -> p $ "[Mismi internal error] - " <> t
  Target a o -> p $ "[Mismi internal error] - " <> "Can not copy [" <> addressToText a <> "] to [" <> addressToText o <> "]. Target file exists"
  AwsErr e -> err e
  UnknownErr e -> se e

retryAWSAction :: RetryPolicy -> AWS a -> AWS a
retryAWSAction rp a = do
  local (retryAWS' rp) $ a

retryAWS :: Int -> Env -> Env
retryAWS i = retryAWS' (retryWithBackoff i)

retryAWS' :: RetryPolicy -> Env -> Env
retryAWS' r e =
  let err c v = case v of
        NoResponseDataReceived -> pure True
        StatusCodeException s _ _ -> pure $ s == status500
        FailedConnectionException _ _ -> pure True
        FailedConnectionException2 _ _ _ _ -> pure True
        TlsException _ -> pure True
        _ -> (e ^. envRetryCheck) c v
  in
  e & envRetryPolicy .~ Just r & envRetryCheck .~ err

sse :: ServerSideEncryption
sse =
  AES256
