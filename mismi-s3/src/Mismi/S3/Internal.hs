{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mismi.S3.Internal (
    f'
  , ff'
  , calculateChunks
  , downRange
  ) where

import           Data.Text

import           P

import           Mismi.S3.Data

f' :: (Text -> Text -> a) -> Address -> a
f' f a =
  uncurry f (unBucket $ bucket a, unKey $ key a)

ff' :: (Text -> Text -> a) -> Address -> a
ff' f a =
  uncurry f (unKey $ key a, unBucket $ bucket a)

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

-- http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35
-- https://github.com/aws/aws-sdk-java/blob/master/aws-java-sdk-s3/src/main/java/com/amazonaws/services/s3/AmazonS3Client.java#L1135
downRange :: Int -> Int -> Text
downRange start end =
  pack $ "bytes=" <> show start <> "-" <> show end
