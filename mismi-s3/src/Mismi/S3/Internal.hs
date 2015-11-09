{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Mismi.S3.Internal (
    fencode'
  , calculateChunks
  , calculateChunksCapped
  , downRange
  , sinkChan
  , sinkChanWithDelay
  , sinkQueue
  , waitForNResults
  , withFileSafe
  ) where

import           Control.Concurrent
import           Control.Retry

import           Control.Monad.Catch
import           Control.Monad.IO.Class

import           Data.Conduit
import qualified Data.Conduit.List as DC

import           Data.Text hiding (length)
import           Data.UUID
import           Data.UUID.V4

import           P

import           Mismi (AWS, rawRunAWS)
import           Mismi.Amazonka (Env)
import           Mismi.S3.Data
import           Network.AWS.S3

import           System.Directory
import           System.IO
import           System.FilePath

import           Twine.Data


fencode' :: (BucketName -> ObjectKey -> a) -> Address -> a
fencode' f (Address (Bucket b) k) =
  BucketName b `f` ObjectKey (unKey k)


calculateChunksCapped :: Int -> Int -> Int -> [(Int, Int, Int)]
calculateChunksCapped size chunk' capped =
  let s = calculateChunks size chunk' in
  if length s > capped
    then calculateChunksCapped size (chunk' * 2) capped
    else s

-- filesize -> Chunk -> [(offset, chunk, index)]
calculateChunks :: Int -> Int -> [(Int, Int, Int)]
calculateChunks size chunk' =
  let chunk = max 1 chunk'
      go :: Int -> Int -> [(Int, Int, Int)]
      go !i o =
        let !o' = (o + chunk) in
          if (o' < size)
            then
              (o, chunk, i) : go (i + 1) o'
            else
              let !c' = (size - o) in -- last chunk
              [(o, c', i)]
  in
    go 1 0

-- http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35
-- https://github.com/aws/aws-sdk-java/blob/master/aws-java-sdk-s3/src/main/java/com/amazonaws/services/s3/AmazonS3Client.java#L1135
downRange :: Int -> Int -> Text
downRange start end =
  pack $ "bytes=" <> show start <> "-" <> show end

sinkChan :: MonadIO m => Source m a -> Chan a -> m Int
sinkChan source c =
  sinkChanWithDelay 0 source c

sinkChanWithDelay :: MonadIO m => Int -> Source m a -> Chan a -> m Int
sinkChanWithDelay delay source c =
  source $$ DC.foldM (\i v -> liftIO $ threadDelay delay >> writeChan c v >> pure (i + 1)) 0

sinkQueue :: Env -> Source AWS a -> Queue a -> IO ()
sinkQueue e source q =
  rawRunAWS e (source $$ DC.mapM_ (liftIO . writeQueue q))


waitForNResults :: Int -> Chan a -> IO [a]
waitForNResults i c = do
  let waitForDone acc =
        if (length acc == i)
           then pure acc
           else do
             r <- readChan c
             waitForDone (r : acc)
  waitForDone []

-- | Create a temporary file location that can be used safely, and on a successful operation, do an (atomic) rename
-- NOTE: This function requires that the `FilePath` provided in the callback exists, otherwise throws an exception
withFileSafe :: (MonadCatch m, MonadIO m) => FilePath -> (FilePath -> m a) -> m a
withFileSafe f1 run = do
  uuid <- liftIO nextRandom >>= return . toString
  let f2 = takeDirectory f1 <> "/" <> "." <> takeFileName f1 <> "." <> uuid
  onException
    (run f2 >>= \a -> liftIO (renameFile f2 f1) >> return a)
    (liftIO $ removeFile f2)
