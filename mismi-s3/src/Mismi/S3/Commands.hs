{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Mismi.S3.Commands (
    exists
  , delete
  , read
  , headObject
  , download
  , downloadWithMode
  , multipartDownload
  , upload
  , calculateChunks
  , write
  , writeWithMode
  , copy
  , move
  , getObjects
  , listObjects
  , list
  , getObjectsRecursively
  , listRecursively
  , getSize
  , sync
  ) where

import qualified Aws.S3 as S3
import           Aws.S3 hiding (headObject, putObject)

import           Control.Arrow ((***))

import           Control.Concurrent.MSem
import           Control.Concurrent.Async

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Catch (catch)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List as C
import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Text.Encoding as T

import           Mismi.Control
import qualified Mismi.S3.Amazonka as AWS
import           Mismi.S3.Control
import           Mismi.S3.Data
import           Mismi.S3.Internal

import           Network.HTTP.Conduit (responseBody, requestBodySource , RequestBody(..))
import           Network.HTTP.Types.Status (status404)

import           P

import           Prelude (error)

import           System.IO
import           System.FilePath
import           System.Directory

import           X.Data.Conduit.Binary

sse :: ServerSideEncryption
sse =
  AES256

exists :: Address -> S3Action Bool
exists a =
  headObject a >>= pure . isJust

headObject :: Address -> S3Action (Maybe S3.ObjectMetadata)
headObject a =
  awsRequest (f' S3.headObject a) >>= pure . S3.horMetadata

getSize :: Address -> S3Action (Maybe Int)
getSize a =
  ifM (exists a) (liftAWSAction $ AWS.getSize a) (pure Nothing)

delete :: Address -> S3Action ()
delete a =
  void . awsRequest $ ff' S3.DeleteObject a

read :: Address -> S3Action (Maybe Text)
read a =
  let get = f' S3.getObject a in
  (awsRequest get >>=
   fmap Just . lift . fmap (T.decodeUtf8 . BS.concat) . ($$+- C.consume) . responseBody . S3.gorResponse)
  `catch` (\(e :: S3.S3Error) -> if S3.s3StatusCode e == status404 then pure Nothing else throwM e)

download :: Address -> FilePath -> S3Action ()
download a p =
  downloadWithMode Fail a p

downloadWithMode :: WriteMode -> Address -> FilePath -> S3Action ()
downloadWithMode mode a p = do
  when (mode == Fail) . whenM (liftIO $ doesFileExist p) . fail $ "Can not download to a target that already exists [" <> p <> "]."
  unlessM (exists a) . fail $ "Can not download when the source does not exist [" <> (T.unpack $ addressToText a) <> "]."
  liftIO $ createDirectoryIfMissing True (dropFileName p)
  s' <- getSize a
  size <- maybe (fail $ "Can not calculate the file size [" <> (T.unpack $ addressToText a) <> "].") pure s'
  if (size > 200 * 1024 * 1024)
     then
       multipartDownload a p size 100 100
     else
       awsRequest (f' S3.getObject a) >>= lift . ($$+- sinkFile p) . responseBody . S3.gorResponse

multipartDownload :: Address -> FilePath -> Int -> Integer -> Int -> S3Action ()
multipartDownload source destination size chunk' fork = do
  -- get config / region to run currently
  (cfg, s3', _) <- ask
  r <- maybe (fail $ "Invalid s3 endpoint [" <> (show $ s3Endpoint s3') <> "].") pure (epToRegion $ s3Endpoint s3')

  -- chunks
  let chunk = chunk' * 1024 * 1024
  let chunks = calculateChunks size (fromInteger chunk)

  let writer :: (Int, Int, Int) -> IO ()
      writer (o, c, _) = do
        let req = downloadPart source o (o + c) destination
            ioq = runS3WithCfg cfg r req
        retryHttp 3 ioq

  -- create sparse file
  liftIO $ withFile destination WriteMode $ \h ->
    hSetFileSize h (toInteger size)

  sem <- liftIO $ new fork
  z <- liftIO $ (mapConcurrently (with sem . writer) chunks)
  pure $ mconcat z

downloadPart :: Address -> Int -> Int -> FilePath -> S3Action ()
downloadPart source start end destination =
  liftAWSAction $ AWS.downloadWithRange source start end destination

upload :: FilePath -> Address -> S3Action ()
upload file a = do
  whenM (exists a) . fail $ "Can not upload to a target that already exists [" <> (T.unpack $ addressToText a) <> "]."
  unlessM (liftIO $ doesFileExist file) . fail $ "Can not upload when the source does not exist [" <> file <> "]."
  s <- liftIO $ withFile file ReadMode $ \h ->
    hFileSize h
  let chunk = 100 * 1024 * 1024
  if s < chunk
    then do
      upload' file a
    else do
      if (s > 1024 * 1024 * 1024)
         then multipartUpload' file a s (10 * chunk)
         else multipartUpload' file a s chunk

upload' :: FilePath -> Address -> S3Action ()
upload' file a = do
  x <- liftIO $ LBS.readFile file
  void . awsRequest $ putObject a (RequestBodyLBS x) sse

multipartUpload' :: FilePath -> Address -> Integer -> Integer -> S3Action ()
multipartUpload' file a fileSize chunk = do
  let mpu = (f' S3.postInitiateMultipartUpload a) { imuServerSideEncryption = Just sse }
  mpur <- awsRequest mpu
  (cfg, scfg, mgr) <- ask
  let upi :: Text = S3.imurUploadId mpur
  let p = calculateChunks (fromInteger fileSize) (fromInteger chunk)
  let x :: (Int, Int, Int) -> IO (Either S3.S3Error S3.UploadPartResponse) = (\(o :: Int, c :: Int, i :: Int) -> do
            let body = requestBodySource (fromInteger . toInteger $ c) $
                  slurpWithBuffer file (toInteger o) (Just $ toInteger c) (1024 * 1024)
            let up = (f' S3.uploadPart a (toInteger i) upi body)
            let runUpPart = flip runReaderT (cfg, scfg, mgr) $ awsRequest up
            let res = (runResourceT runUpPart >>= pure . Right) `catch` (\(e :: S3.S3Error) -> pure . Left $ e)
            retryHttp 3 res
          )
  prts <- liftIO (mapConcurrently x p)
  case sequence prts of
    Left _ ->
      void . awsRequest $ f' S3.postAbortMultipartUpload a upi
    Right prts' -> do
      let prts'' = (uncurry (\(_, _, i) pr -> (toInteger i, uprETag pr))) <$> L.zip p prts'
      void . awsRequest $ (f' S3.postCompleteMultipartUpload a upi prts'')

write :: Address -> Text -> S3Action ()
write =
  writeWithMode Fail

writeWithMode :: WriteMode -> Address -> Text -> S3Action ()
writeWithMode w a t = do
  case w of
    Fail        -> whenM (exists a) . fail $ "Can not write to a file that already exists [" <> show a <> "]."
    Overwrite   -> return ()
  let body = RequestBodyBS $ T.encodeUtf8 t
  void . awsRequest $ putObject a body sse

copy :: Address -> Address -> S3Action ()
copy s d =
  liftAWSAction $ AWS.copy s d

move :: Address -> Address -> S3Action ()
move source destination =
  copy source destination >>
    delete source

putObject :: Address -> RequestBody -> S3.ServerSideEncryption -> S3.PutObject
putObject a body e =
  (f' S3.putObject a body) { S3.poServerSideEncryption = Just e }

-- pair of prefixs and keys
getObjects :: Address -> S3Action ([Key], [Key])
getObjects (Address (Bucket buck) (Key ky)) =
  ((Key <$>) *** (Key <$>)) <$> (ff $ (S3.getBucket buck) { S3.gbPrefix = Just $ pp ky, S3.gbDelimiter = Just $ "/" })
  where
    pp :: Text -> Text
    pp k = if T.null k then "" else if T.isSuffixOf "/" k then k else k <> "/"
    ff :: S3.GetBucket -> S3Action ([T.Text], [T.Text])
    ff b = do
      r <- awsRequest b
      if S3.gbrIsTruncated r
        then
        do
          let d = (S3.gbrCommonPrefixes r, S3.objectKey <$> S3.gbrContents r)
          n <- ff $ b { S3.gbMarker = S3.gbrNextMarker r }
          pure $ (d <> n)
        else
        pure $ (S3.gbrCommonPrefixes r, S3.objectKey <$> S3.gbrContents r)

-- Pair of list of prefixes and list of keys
listObjects :: Address -> S3Action ([Address], [Address])
listObjects a =
  (\(p, k) -> (Address (bucket a) <$> p, Address (bucket a) <$> k) )<$> getObjects a

-- list the addresses, keys first, then prefixes
list :: Address -> S3Action [Address]
list a =
  (\(p, k) -> k <> p) <$> listObjects a

getObjectsRecursively :: Address -> S3Action [S3.ObjectInfo]
getObjectsRecursively (Address (Bucket b) (Key ky)) =
  getObjects' $ (S3.getBucket b) { S3.gbPrefix = Just $ pp ky }
  where
    pp :: Text -> Text
    pp k = if T.null k then "" else if T.isSuffixOf "/" k then k else k <> "/"
    -- Hoping this will have ok performance in cases where the results are large, it shouldnt
    -- affect correctness since we search through the list for it anyway
    go :: S3.GetBucket -> NEL.NonEmpty S3.ObjectInfo -> S3Action [S3.ObjectInfo]
    go x ks = (NEL.toList ks <>) <$> getObjects' (x { S3.gbMarker = Just $ S3.objectKey $ NEL.last ks })
    getObjects' :: S3.GetBucket -> S3Action [S3.ObjectInfo]
    getObjects' x = do
      resp <- awsRequest x
      if S3.gbrIsTruncated resp
        then
          maybe
            (Prelude.error "vee: error: truncated response with empty contents list.")
            (go x)
            (NEL.nonEmpty $ S3.gbrContents resp)
        else
          pure $ S3.gbrContents resp

listRecursively :: Address -> S3Action [Address]
listRecursively a =
  fmap (Address (bucket a) . Key . S3.objectKey) <$> getObjectsRecursively a

sync :: SyncMode -> Address -> Address -> Int -> S3Action ()
sync mode source dest fork =
  liftAWSAction $ AWS.syncWithMode mode source dest fork
