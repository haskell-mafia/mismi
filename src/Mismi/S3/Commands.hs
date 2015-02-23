{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mismi.S3.Commands (
    getTextUtf8
  , getObjects
  ) where

import qualified Aws.S3 as S3

import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Catch (catch, throwM)

import           Data.ByteString as BS
import           Data.Conduit
import           Data.Conduit.List as C
import qualified Data.List.NonEmpty as NEL
import           Data.Text as T
import           Data.Text.Encoding as T

import           Mismi.Control
import           Mismi.S3.Control
import           Mismi.S3.Data

import           Network.HTTP.Conduit (responseBody)
import           Network.HTTP.Types.Status (status404)

import           P

import           Prelude (error)


getTextUtf8 :: Address -> S3Action (Maybe Text)
getTextUtf8 (Address (Bucket b) k) =
  (awsRequest (S3.getObject b (showKey k)) >>=
    fmap Just . lift . fmap (T.decodeUtf8 . BS.concat) . ($$+- C.consume) . responseBody . S3.gorResponse)
    `catch` (\(e :: S3.S3Error) -> if S3.s3StatusCode e == status404 then pure Nothing else throwM e)

getObjects :: Address -> S3Action [S3.ObjectInfo]
getObjects (Address (Bucket b) k) = getObjects' $ (S3.getBucket b) { S3.gbPrefix = Just $ showKey k <> "/" }
  where
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
