{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Mismi.S3.Amazonka (
    module AWS
  , headObject
  , getSize'
  , copy
  , upload
  , download
  ) where

import           Control.Lens
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Resource
import           Control.Monad.IO.Class

import           Data.Conduit
import           Data.Conduit.Binary

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import           Data.Text.Encoding as T

import           Mismi.S3.Data
import           Mismi.S3.Internal

import           Network.AWS.S3 hiding (headObject, bucket)
import qualified Network.AWS.S3 as AWS
import           Network.AWS.Data
import           Network.HTTP.Types.URI (urlEncode)

import           P

import           System.IO
import           System.Directory
import           System.FilePath

headObject :: Address -> AWST IO (AWS.HeadObjectResponse)
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
      req = (AWS.copyObject b (sb <> "/" <> sk) textEncoded) & AWS.coServerSideEncryption .~ Just sse & AWS.coMetadataDirective .~ Just AWS.Copy
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

sse :: ServerSideEncryption
sse =
  AES256
