{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mismi.S3.Patch.Network (
    chunkedFile
  ) where

import           Control.Monad
import           Control.Monad.Trans.Resource
import           Control.Monad.IO.Class

import           Data.Conduit

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import           Mismi.Amazonka (getFileSize, unsafeChunkedBody)

import           Network.AWS.Data.Body

import           P

import           System.IO

chunkedFile :: MonadIO m => ChunkSize -> FilePath -> m RqBody
chunkedFile c f = do
    n <- getFileSize f
    if n > toInteger c
      then
        return $ unsafeChunkedBody c n (sourceFileChunks c f)
      else
        liftIO $ (=<<) (return . toBody) $ LBS.readFile f

-- Uses hGet with a specific buffer size, instead of hGetSome.
sourceFileChunks :: MonadResource m
                 => ChunkSize
                 -> FilePath
                 -> Source m BS.ByteString
sourceFileChunks (ChunkSize sz) f =
  bracketP (openBinaryFile f ReadMode) hClose go
  where
    go h = do
        bs <- liftIO (BS.hGet h sz)
        unless (BS.null bs) $ do
            yield bs
            go h
