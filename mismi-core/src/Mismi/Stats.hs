{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-cse #-}
module Mismi.Stats
  ( Stats (..)
  , initStats
  , finalizeStats
  , reportStats

  , logHttpException
  , logIOException
  ) where

import           Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef)
import           Data.Time.Clock (UTCTime (..), diffUTCTime, getCurrentTime)
import           Data.Time.Calendar (fromGregorian)

import           P

import           System.IO (IO, putStrLn)
import           System.IO.Unsafe (unsafePerformIO)


data Stats = Stats
  { downloadSize :: !Int
  , chunkCount :: !Int
  , concurrency :: !Int
  , maxChunkSize :: !Int
  , httpExceptionRetryCount :: !Int
  , ioExceptionRetryCount :: !Int
  , startTime :: !UTCTime
  , endTime :: !UTCTime
  }

{-# NOINLINE stats #-}
stats :: IORef Stats
stats = unsafePerformIO $ newIORef (Stats 0 0 0 0 0 0 utcZero utcZero)

utcZero :: UTCTime
utcZero = UTCTime (fromGregorian 2000 1 1) 0

initStats :: Int -> Int -> Int -> Int -> IO ()
initStats fsize chunks threads chunkSize = do
  now <- getCurrentTime
  atomicModifyIORef' stats $ \ _ ->
    (Stats fsize chunks threads chunkSize 0 0 now utcZero, ())

finalizeStats :: IO Stats
finalizeStats = do
  now <- getCurrentTime
  res <- readIORef stats
  pure $ res { endTime = now }

logHttpException :: IO ()
logHttpException =
  atomicModifyIORef' stats $ \ s ->
    (s { httpExceptionRetryCount = 1 + httpExceptionRetryCount s}, ())

logIOException :: IO ()
logIOException =
  atomicModifyIORef' stats $ \ s ->
    (s { ioExceptionRetryCount = 1 + ioExceptionRetryCount s}, ())

reportStats :: Stats -> IO ()
reportStats s = do
  let duration = diffUTCTime (endTime s) (startTime s)
  putStrLn $ mconcat
    [ "File size : ", show (downloadSize s), "\n"
    , "Chunk count : ", show (chunkCount s), "\n"
    , "Concurrency : ", show (concurrency s), "\n"
    , "Max chunk size : " , show (maxChunkSize s), "\n"
    , "HTTP Retries : ", show (httpExceptionRetryCount s), "\n"
    , "IO Retries : ", show (ioExceptionRetryCount s), "\n"
    , "Duration : ", show duration, "\n"
    ]
