{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mismi.S3.Commands (
    exists
  , delete
  , read
  , download
  , upload
  , calculateChunks
  , write
  , getObjects
  , listRecursively
  ) where

import qualified Aws.S3 as S3
import           Aws.S3 hiding (putObject)

import           Control.Concurrent.Async

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Catch (Handler (..), catch)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource


import           Control.Retry

import           Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Conduit
import           Data.Conduit.Binary (sinkFile)
import qualified Data.Conduit.List as C
import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL
import           Data.Text as T
import           Data.Text.Encoding as T

import           Mismi.Control
import           Mismi.S3.Control
import           Mismi.S3.Data

import           Network.HTTP.Conduit (responseBody, RequestBody(..))
import           Network.HTTP.Client (HttpException)
import           Network.HTTP.Types.Status (status404)

import           P

import           Prelude (error)

import           System.IO

import           System.FilePath
import           System.Directory

f' :: (Text -> Text -> a) -> Address -> a
f' f a =
  uncurry f (unBucket $ bucket a, unKey $ key a)

ff' :: (Text -> Text -> a) -> Address -> a
ff' f a =
  uncurry f (unKey $ key a, unBucket $ bucket a)

exists :: Address -> S3Action Bool
exists a =
  let req = f' S3.headObject a in
  awsRequest req >>= pure . isJust . S3.horMetadata

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
  let get = f' S3.getObject a in do
    whenM (liftIO $ doesFileExist p) . fail $ "Can not download to a target that already exists [" <> p <> "]."
    unlessM (exists a) . fail $ "Can not download when the source does not exist [" <> (T.unpack $ addressToText a) <> "]."
    liftIO $ createDirectoryIfMissing True (dropFileName p)
    awsRequest get >>= lift . ($$+- sinkFile p) . responseBody . S3.gorResponse

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
  let x :: (Int, Int, Int) -> IO (Either S3.S3Error S3.UploadPartResponse) = (\(o :: Int, c :: Int, i :: Int) ->
            withFile file ReadMode $ \h -> do
              hSeek h AbsoluteSeek (toInteger o)
              cont <- LBS.hGetContents h
              let body = RequestBodyLBS (LBS.take (fromInteger . toInteger $ c) cont)
              let up = (f' S3.uploadPart a (toInteger i) upi body)
              let fail' = \(e :: S3.S3Error) -> pure $ Left e
              let runUpPart = flip runReaderT (cfg,scfg, mgr) $ awsRequest up
              let res = runResourceT $ fmap Right runUpPart `catch` fail'
              recovering (limitRetries 3) ([const $ Handler (\(_ :: HttpException) -> pure True) ]) res
          )
  prts <- liftIO $ mapConcurrently x p
  case sequence prts of
    Left _ -> do
      void . awsRequest $ f' S3.postAbortMultipartUpload a upi
    Right prts' -> do
      let prts'' = (uncurry (\(_, _, i) pr -> (toInteger i, uprETag pr))) <$> L.zip p prts'
      void . awsRequest $ (f' S3.postCompleteMultipartUpload a upi prts'')

-- filesize -> Chunk -> [(offset, chunk, index)]
calculateChunks :: Int -> Int -> [(Int, Int, Int)]
calculateChunks size chunk =
  let go :: Int -> Int -> [(Int, Int, Int)]
      go i o =
        let o' = (o + chunk) in
          if (o' < size)
            then
              (o, chunk, i) : go (i + 1) o'
            else
              let c' = (size - o) in -- last chunk
              [(o, c', i)]
  in
    go 1 0

write :: WriteMode -> Address -> Text -> S3Action ()
write w a t = do
  case w of
    Fail        -> whenM (exists a) . fail $ "Can not write to a file that already exists [" <> show a <> "]."
    Overwrite   -> return ()
  let body = RequestBodyBS $ T.encodeUtf8 t
  void . awsRequest $ putObject a body sse

putObject :: Address -> RequestBody -> S3.ServerSideEncryption -> S3.PutObject
putObject a body e =
  (f' S3.putObject a body) { S3.poServerSideEncryption = Just e }

getObjects :: Address -> S3Action [S3.ObjectInfo]
getObjects (Address (Bucket b) (Key ky)) =
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
            (error "vee: error: truncated response with empty contents list.")
            (go x)
            (NEL.nonEmpty $ S3.gbrContents resp)
        else
          pure $ S3.gbrContents resp

listRecursively :: Address -> S3Action [Address]
listRecursively a =
  fmap (Address (bucket a) . Key . S3.objectKey) <$> getObjects a
