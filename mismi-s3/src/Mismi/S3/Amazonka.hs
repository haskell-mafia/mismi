{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PackageImports #-}
module Mismi.S3.Amazonka (
    module AWS
  , headObject
  , exists
  , getSize
  , delete
  , read
  , copy
  , move
  , upload
  , download
  , downloadWithMode
  , downloadWithRange
  , listMultiparts
  , listOldMultiparts
  , listOldMultiparts'
  , abortMultipart
  , abortMultipart'
  , filterOld
  , filterNDays
  , sse
  ) where

import           Control.Concurrent.MSem
import           Control.Concurrent.Async

import           Control.Lens
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Either
import           Control.Monad.Reader (ask)
import           Control.Monad.Trans.Resource
import           Control.Monad.IO.Class

import           Data.Conduit
import qualified Data.Conduit.List as DC
import           Data.Conduit.Binary
import qualified Data.HashMap.Strict as HM


import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import           Data.String
import qualified Data.Text as T
import           Data.Text.Encoding as T
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Time.Clock

import           Mismi.Control.Amazonka (unsafeAWS, runAWSWithEnv, awsErrorRender)
import           Mismi.S3.Control
import           Mismi.S3.Data
import           Mismi.S3.Internal

import           Network.AWS.S3 hiding (headObject, Bucket, bucket)
import qualified Network.AWS.S3 as AWS
import           Network.AWS.Data
import           Network.HTTP.Types (urlEncode)
import           Network.HTTP.Types.Status (status500, status404)
import           Network.HTTP.Client ( HttpException (..) )

import           Network.HTTP.Conduit (requestBodySource) -- maybe
import           X.Data.Conduit.Binary

import           P

import           System.IO
import           System.Directory
import           System.FilePath
import           System.Posix.IO
import qualified "unix-bytestring" System.Posix.IO.ByteString as UBS

headObject :: Address -> AWS (Maybe AWS.HeadObjectResponse)
headObject a = do
  res <- sendCatch $ f' AWS.headObject a
  handle404 res

exists :: Address -> AWS Bool
exists a =
  headObject a >>= pure . maybe False (\z -> not $ HM.null (z ^. horMetadata))

getSize :: Address -> AWS (Maybe Int)
getSize a =
  headObject a >>= pure . maybe Nothing (^. horContentLength)

delete :: Address -> AWS ()
delete =
  send_ . f' deleteObject

handle404 :: Either (ServiceError RESTError) (a) -> AWS (Maybe a)
handle404 res = case res of
  Left e -> case e of
    HttpError e' -> case e' of
      StatusCodeException s _ _ ->
        if s == status404
          then pure Nothing
          else throwAWSError e
      _ -> throwAWSError e
    SerializerError _ _ -> throwAWSError e
    ServiceError _ _ _ -> throwAWSError e
    Errors _ -> throwAWSError e
  Right rs -> pure $ Just rs

getObject' :: Address -> AWS (Maybe GetObjectResponse)
getObject' a = do
  let req = f' getObject a
  res <- sendCatch req
  handle404 res

read :: Address -> AWS (Maybe Text)
read a = do
  resp <- getObject' a
  let format y = T.concat . TL.toChunks . TL.decodeUtf8 $ y
  z <- liftIO . sequence $ (\r -> runResourceT (r ^. gorBody . _RsBody $$+- sinkLbs)) <$> resp
  pure (format <$> z)


-- Url is being sent as a header not as a query therefore
-- requires special url encoding. (Do not encode the delimiters)
copy :: Address -> Address -> AWS ()
copy (Address (Bucket sb) (Key sk)) (Address (Bucket b) (Key k)) =
  let splitEncoded = urlEncode True . T.encodeUtf8 <$> T.split (== '/') k
      bsEncoded = BS.intercalate "/" splitEncoded
      textEncoded = T.decodeUtf8 bsEncoded
      req = (AWS.copyObject b (sb <> "/" <> sk) textEncoded) & AWS.coServerSideEncryption .~ Just sse & AWS.coMetadataDirective .~ Just AWS.Copy
  in
  send_ req

move :: Address -> Address -> AWS ()
move s d =
  copy s d >>
    delete s


upload :: FilePath -> Address -> AWS ()
upload f a = do
  whenM (exists a) . fail $ "Can not upload to a target that already exists [" <> (T.unpack $ addressToText a) <> "]."
  unlessM (liftIO $ doesFileExist f) . fail $ "Can not upload when the source does not exist [" <> f <> "]."
  s <- liftIO $ withFile f ReadMode $ \h ->
    hFileSize h
  let chunk = 100 * 1024 * 1024
  if s < chunk
    then do
      upload' f a
    else do
      if (s > 1024 * 1024 * 1024)
         then multipartUpload' f a s (10 * chunk)
         else multipartUpload' f a s chunk

upload' :: FilePath -> Address -> AWS ()
upload' file a = do
  x <- liftIO $ LBS.readFile file
  send_ $ f' (putObject $ toBody x) a & poServerSideEncryption .~ Just sse

multipartUpload' :: FilePath -> Address -> Integer -> Integer -> AWS ()
multipartUpload' file a fileSize chunk = do
  e <- ask
  mpu' <- send $ f' createMultipartUpload a & cmuServerSideEncryption .~ Just sse
  let mpu = maybe (fail "Failed to create multipart upload") pure (mpu' ^. cmurUploadId)

  let chunks = calculateChunks (fromInteger fileSize) (fromInteger chunk)
  let uploader :: (Int, Int, Int) -> IO ()
      uploader (o, c, i) =
--        let body = slurpWithBuffer file (toInteger o) (Just $ toInteger c) (1024 * 1024)
        withFile file ReadMode $ \h -> do
          hSeek h AbsoluteSeek (toInteger o)
          cont <- LBS.hGetContents h
          let bod = toBody (LBS.take (fromInteger . toInteger $ c) cont)
          let req' = f' (uploadPart bod) a i mpu
          unsafeAWS . runAWSWithEnv e $ send_ req'

  prts <- liftIO $ mapConcurrently x p
  case partitionEithers prts of
    ([], _) -> send_ $ f' abortMultipartUpload a mpu
    (_, _) -> send_ $ f' completeMultipartUpload a mpu
--  send_ $ f' completeMultipartUpload a mpu
--  pure ()

download :: Address -> FilePath -> AWS ()
download a f = do
  unlessM (exists a) . fail $ "Can not download when the source does not exist [" <> (T.unpack $ addressToText a) <> "]."
  s' <- getSize a
  size <- maybe (fail $ "Can not calculate the file size [" <> (T.unpack $ addressToText a) <> "].") pure s'
  if (size > 200 * 1024 * 1024)
     then multipartDownload a f size 100 100
     else downloadWithMode Fail a f

downloadWithMode :: WriteMode -> Address -> FilePath -> AWS ()
downloadWithMode mode a f = do
  when (mode == Fail) . whenM (liftIO $ doesFileExist f) . fail $ "Can not download to a target that already exists [" <> f <> "]."
  unlessM (exists a) . fail $ "Can not download when the source does not exist [" <> (T.unpack $ addressToText a) <> "]."
  liftIO $ createDirectoryIfMissing True (dropFileName f)
  r <- getObject' a
  r' <- maybe (fail "shit") pure r
  liftIO . runResourceT . ($$+- sinkFile f) $ r' ^. gorBody ^. _RsBody

multipartDownload :: Address -> FilePath -> Int -> Integer -> Int -> AWS ()
multipartDownload source destination size chunk' fork = do
  e <- ask

  let chunk = chunk' * 1024 * 1024
  let chunks = calculateChunks size (fromInteger chunk)

  let writer :: (Int, Int, Int) -> IO ()
      writer (o, c, _) = do
        let req = downloadWithRange source o (o + c) destination
            ioq = runAWSWithEnv e req
            io = unsafeAWS ioq
        retryHttp 3 io

  -- create sparse file
  liftIO $ withFile destination WriteMode $ \h ->
    hSetFileSize h (toInteger size)

  sem <- liftIO $ new fork
  z <- liftIO $ (mapConcurrently (with sem . writer) chunks)
  pure $ mconcat z


downloadWithRange :: Address -> Int -> Int -> FilePath -> AWS ()
downloadWithRange source start end dest = do
  let req = (f' AWS.getObject source) & AWS.goRange .~ (Just $ downRange start end)
  r <- send req
  let p :: AWS.GetObjectResponse = r
  let y :: RsBody = p ^. AWS.gorBody

  fd <- liftIO $ openFd dest WriteOnly Nothing defaultFileFlags
  liftIO $ do
    let rs :: ResumableSource (ResourceT IO) BS.ByteString = y ^. _RsBody
    let s = awaitForever $ \bs -> liftIO $ do
              UBS.fdWrite fd bs
    runResourceT $ ($$+- s) rs

  liftIO $ closeFd fd

listMultiparts :: Bucket -> AWS [MultipartUpload]
listMultiparts b = do
  let req = listMultipartUploads $ unBucket b
  paginate req $$ DC.foldMap (flip (^.) lmurUploads)

listOldMultiparts :: Bucket -> AWS [MultipartUpload]
listOldMultiparts b = do
  mus <- listMultiparts b
  now <- liftIO $ getCurrentTime
  pure $ filter (filterOld now) mus

listOldMultiparts' :: Bucket -> Int -> AWS [MultipartUpload]
listOldMultiparts' b i = do
  mus <- listMultiparts b
  now <- liftIO $ getCurrentTime
  pure $ filter (filterNDays i now) mus

filterOld :: UTCTime -> MultipartUpload -> Bool
filterOld n m = filterNDays 7 n m

filterNDays :: Int -> UTCTime -> MultipartUpload -> Bool
filterNDays n now m = case m ^. muInitiated of
  Nothing -> False
  Just x -> nDaysOld n now x

nDaysOld :: Int -> UTCTime -> UTCTime -> Bool
nDaysOld n now utc = do
  let n' = fromInteger $ toInteger n
  let diff = ((-1 * 60 * 60 * 24 * n') :: NominalDiffTime)
  let boundary = addUTCTime diff now
  boundary > utc

abortMultipart :: Bucket -> MultipartUpload -> AWST IO ()
abortMultipart (Bucket b) mu = do
  let x :: String -> ServiceError String = \s -> (ServiceError "amu" status500 s)
  k <- maybe (throwAWSError $ x "Multipart key missing") pure (mu ^. muKey)
  i <- maybe (throwAWSError $ x "Multipart uploadId missing") pure (mu ^. muUploadId)
  abortMultipart' (Address (Bucket b) (Key k)) i

abortMultipart' :: Address -> T.Text -> AWST IO ()
abortMultipart' (Address (Bucket b) (Key k)) i =
  send_ $ abortMultipartUpload b k i


sse :: ServerSideEncryption
sse =
  AES256
