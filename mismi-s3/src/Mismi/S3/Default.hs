{-# LANGUAGE NoImplicitPrelude #-}
{- |
An exposed set of S3 operations that have the default retries/backoff for batch operations.
See "Mismi.S3.Retry" for operations that support arbitrary 'RetryPolicy'.
See "Mismi.S3.Commands" for the raw operations that by-and-large don't retry.
-}
module Mismi.S3.Default (
    exists
  , headObject
  , getSize
  , delete
  , read
  , download
  , downloadWithMode
  , multipartDownload
  , upload
  , write
  , writeWithMode
  , copy
  , move
  , listObjects
  , list
  , getObjectsRecursively
  , listRecursively
  , sync
  ) where

import           Data.Text as T hiding (copy)

import           Mismi.S3.Control
import           Mismi.S3.Data
import qualified Mismi.S3.Retry as M
import           Mismi.S3.Internal

import           P

import           System.IO


exists :: Address -> S3Action Bool
exists = M.exists (retryWithBackoff 5)

headObject :: Address -> S3Action (Maybe ObjectMetadata)
headObject = M.headObject (retryWithBackoff 5)

getSize :: Address -> S3Action (Maybe Int)
getSize = M.getSize (retryWithBackoff 5)

delete :: Address -> S3Action ()
delete = M.delete (retryWithBackoff 5)

read :: Address -> S3Action (Maybe Text)
read = M.read (retryWithBackoff 5)

download :: Address -> FilePath -> S3Action ()
download = M.download (retryWithBackoff 5)

downloadWithMode :: WriteMode -> Address -> FilePath -> S3Action ()
downloadWithMode = M.downloadWithMode (retryWithBackoff 5)

multipartDownload :: Address -> FilePath -> Int -> Integer -> Int -> S3Action ()
multipartDownload = M.multipartDownload (retryWithBackoff 3)

upload :: FilePath -> Address -> S3Action ()
upload = M.upload (retryWithBackoff 3)

write :: Address -> Text -> S3Action ()
write = M.write (retryWithBackoff 5)

writeWithMode :: WriteMode -> Address -> Text -> S3Action ()
writeWithMode = M.writeWithMode (retryWithBackoff 5)

copy :: Address -> Address -> S3Action ()
copy = M.copy (retryWithBackoff 5)

move :: Address -> Address -> S3Action ()
move = M.move (retryWithBackoff 5)

listObjects :: Address -> S3Action ([Address], [Address])
listObjects = M.listObjects (retryWithBackoff 5)

list :: Address -> S3Action [Address]
list = M.list (retryWithBackoff 5)

getObjectsRecursively :: Address -> S3Action [ObjectInfo]
getObjectsRecursively = M.getObjectsRecursively (retryWithBackoff 5)

listRecursively :: Address -> S3Action [Address]
listRecursively = M.listRecursively (retryWithBackoff 5)

sync :: SyncMode -> Address -> Address -> Int -> S3Action ()
sync = M.sync (retryWithBackoff 3)
