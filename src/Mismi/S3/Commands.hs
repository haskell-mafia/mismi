{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mismi.S3.Commands (
    getObjects
  ) where

import qualified Aws.S3 as S3

import qualified Data.List.NonEmpty as NEL

import           Mismi.Control
import           Mismi.S3.Control
import           Mismi.S3.Data

import           P

import           Prelude (error)


getObjects :: Address -> S3Action [S3.ObjectInfo]
getObjects (Address (Bucket b) (Key k)) =
  getObjects' $ (S3.getBucket b) { S3.gbPrefix = Just $ k <> "/" }
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
