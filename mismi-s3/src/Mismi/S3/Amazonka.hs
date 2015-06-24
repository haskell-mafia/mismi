{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PackageImports #-}
module Mismi.S3.Amazonka (
    module AWS
  , headObject
  , getSize'
  , copy
  , upload
  , download
  , downloadWithRange
  , listMultipartParts
  , listMultiparts
  , listOldMultiparts
  , listOldMultiparts'
  , abortMultipart
  , abortMultipart'
  , filterOld
  , filterNDays
  , sse
  ) where

import           Control.Lens
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Resource
import           Control.Monad.IO.Class

import           Data.Conduit
import qualified Data.Conduit.List as DC
import           Data.Conduit.Binary


import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import           Data.String
import qualified Data.Text as T
import           Data.Text.Encoding as T
import           Data.Time.Clock

import           Mismi.S3.Data
import           Mismi.S3.Internal

import           Network.AWS.S3 hiding (headObject, Bucket, bucket)
import qualified Network.AWS.S3 as AWS
import           Network.AWS.Data
import           Network.HTTP.Types.URI (urlEncode)
import           Network.HTTP.Types.Status (status500)

import           P

import           System.IO
import           System.Directory
import           System.FilePath
import           System.Posix.IO
import qualified "unix-bytestring" System.Posix.IO.ByteString as UBS

headObject :: Address -> AWST IO AWS.HeadObjectResponse
headObject =
  send . f' AWS.headObject

-- unsafe
getSize' :: Address -> AWST IO (Maybe Int)
getSize' a =
  headObject a >>= pure . (^. horContentLength)

-- Url is being sent as a header not as a query therefore
-- requires special url encoding. (Do not encode the delimiters)
copy :: Address -> Address -> AWS ()
copy (Address (Bucket sb) (Key sk)) (Address (Bucket b) (Key k)) =
  let splitEncoded = urlEncode True . T.encodeUtf8 <$> T.split (== '/') k
      bsEncoded = BS.intercalate "/" splitEncoded
      textEncoded = T.decodeUtf8 bsEncoded
      req = AWS.copyObject b (sb <> "/" <> sk) textEncoded & AWS.coServerSideEncryption .~ Just sse & AWS.coMetadataDirective .~ Just AWS.Copy
  in
  send_ req

upload :: FilePath -> Address -> AWS ()
upload f a = do
   x <- liftIO $ LBS.readFile f
   send_ $ f' (putObject $ toBody x) a & poServerSideEncryption .~ Just sse

download :: Address -> FilePath -> AWS ()
download a f = do
  liftIO $ createDirectoryIfMissing True (dropFileName f)
  r <- send $ f' getObject a
  liftIO . runResourceT . ($$+- sinkFile f) $ r ^. gorBody ^. _RsBody

downloadWithRange :: Address -> Int -> Int -> FilePath -> AWS ()
downloadWithRange source start end dest = do
  let req = f' AWS.getObject source & AWS.goRange .~ (Just $ downRange start end)
  r <- send req
  let p :: AWS.GetObjectResponse = r
  let y :: RsBody = p ^. AWS.gorBody


  fd <- liftIO $ openFd dest WriteOnly Nothing defaultFileFlags
  liftIO $ do
    let rs :: ResumableSource (ResourceT IO) BS.ByteString = y ^. _RsBody
    let s = awaitForever $ \bs -> liftIO $
              UBS.fdWrite fd bs
    runResourceT $ ($$+- s) rs
  liftIO $ closeFd fd

listMultipartParts :: Bucket -> Key -> T.Text -> AWS [Part]
listMultipartParts b k uploadId = do
  let req = AWS.listParts (unBucket b) (unKey k) uploadId
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

abortMultipart :: Bucket -> MultipartUpload -> AWST IO ()
abortMultipart (Bucket b) mu = do
  let x :: String -> ServiceError String = ServiceError "amu" status500
  k <- maybe (throwAWSError $ x "Multipart key missing") pure (mu ^. muKey)
  i <- maybe (throwAWSError $ x "Multipart uploadId missing") pure (mu ^. muUploadId)
  abortMultipart' (Address (Bucket b) (Key k)) i

abortMultipart' :: Address -> T.Text -> AWST IO ()
abortMultipart' (Address (Bucket b) (Key k)) i =
  send_ $ abortMultipartUpload b k i


sse :: ServerSideEncryption
sse =
  AES256
