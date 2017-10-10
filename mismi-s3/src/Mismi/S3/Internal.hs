{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Mismi.S3.Internal (
    f'
  , calculateChunks
  , calculateChunksCapped
  , bytesRange
  , sinkChan
  , sinkChanWithDelay
  , sinkQueue
  , waitForNResults
  , withFileSafe
  ) where

import           Control.Concurrent (Chan, readChan, threadDelay, writeChan)

import           Control.Monad.Catch (MonadCatch, onException)
import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Data.Conduit (Source, ($$))
import qualified Data.Conduit.List as DC

import qualified Data.Text as T
import           Data.UUID (toString)
import           Data.UUID.V4 (nextRandom)

import           P

import           Mismi (AWS, rawRunAWS)
import           Mismi.Amazonka (Env)
import           Mismi.S3.Data
import           Network.AWS.S3 (BucketName (..), ObjectKey (..))

import           System.Directory (renameFile, removeFile)
import           System.IO (IO)
import           System.FilePath (FilePath, takeDirectory, takeFileName)

import           Twine.Data (Queue, writeQueue)


f' :: (BucketName -> ObjectKey -> a) -> Address -> a
f' f (Address (Bucket b) k) =
  BucketName b `f` ObjectKey (unKey k)


calculateChunksCapped :: Int -> Int -> Int -> [(Int, Int, Int)]
calculateChunksCapped size chunk' capped =
  calculateChunks size cappedChunk
  where
    minChunk = ceiling $ size' / capped'

    cappedChunk = max chunk' minChunk

    size' :: Double
    size' = fromIntegral size

    capped' :: Double
    capped' = fromIntegral capped

-- filesize -> Chunk -> [(offset, chunk, index)]
calculateChunks :: Int -> Int -> [(Int, Int, Int)]
calculateChunks size chunk' =
  let chunk = max 1 chunk'
      go :: Int -> Int -> [(Int, Int, Int)]
      go !index offset =
        let !offset' = (offset + chunk) in
          if (offset' < size)
            then
              (offset, chunk, index) : go (index + 1) offset'
            else
              let !c' = (size - offset) in -- last chunk
              [(offset, c', index)]
  in
    go 1 0

-- http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35
-- https://github.com/aws/aws-sdk-java/blob/master/aws-java-sdk-s3/src/main/java/com/amazonaws/services/s3/AmazonS3Client.java#L1135
bytesRange :: Int -> Int -> Text
bytesRange start end =
  T.pack $ "bytes=" <> show start <> "-" <> show end

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
