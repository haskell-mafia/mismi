{-# LANGUAGE NoImplicitPrelude #-}
{- |
An exposed set of S3 operations that have the default retries/backoff for batch operations.
See "Mismi.S3.Commands" for the raw operations that will use default, global retry.
-}
module Mismi.S3.Aws.Default (
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
  , listRecursively
  , getObjectsRecursively
  , sync
  ) where

import           Data.Text as T hiding (copy)

import           Mismi.S3.Aws.Control
import           Mismi.S3.Aws.Data
import qualified Mismi.S3.Aws.Commands as M
import qualified Mismi.S3.Commands as A
import           Mismi.S3.Data
import           Mismi.S3.Internal

import           P

import           System.IO


exists :: Address -> S3Action Bool
exists = retryAction 5 . M.exists

headObject :: Address -> S3Action (Maybe ObjectMetadata)
headObject = retryAction 5 . M.headObject

getSize :: Address -> S3Action (Maybe Int)
getSize = retryAction 5 . M.getSize

delete :: Address -> S3Action ()
delete = retryAction 5 . M.delete

read :: Address -> S3Action (Maybe Text)
read = retryAction 5 . M.read

download :: Address -> FilePath -> S3Action ()
download a = retryAction 5 . M.download a

downloadWithMode :: WriteMode -> Address -> FilePath -> S3Action ()
downloadWithMode m a = retryAction 5 . M.downloadWithMode m a

multipartDownload :: Address -> FilePath -> Int -> Integer -> Int -> S3Action ()
multipartDownload a s d sz = retryAction 3 . M.multipartDownload a s d sz

upload :: FilePath -> Address -> S3Action ()
upload f = retryAction 3 . M.uploadCheck f

write :: Address -> Text -> S3Action ()
write a = retryAction 5 . M.write a

writeWithMode :: WriteMode -> Address -> Text -> S3Action ()
writeWithMode m a = retryAction 5 . M.writeWithMode m a

copy :: Address -> Address -> S3Action ()
copy s = retryAction 5 . M.copy s

move :: Address -> Address -> S3Action ()
move s = retryAction 5 . M.move s

listObjects :: Address -> S3Action ([Address], [Address])
listObjects = retryAction 5 . M.listObjects

list :: Address -> S3Action [Address]
list = retryAction 5 . M.list

getObjectsRecursively :: Address -> S3Action [ObjectInfo]
getObjectsRecursively = retryAction 5 . M.getObjectsRecursively

listRecursively :: Address -> S3Action [Address]
listRecursively = retryAction 5 . M.listRecursively

sync :: SyncMode -> Address -> Address -> Int -> S3Action ()
sync m s d = retryAction 3 . M.sync m s d


retryAction :: Int -> S3Action a -> S3Action a
retryAction r = A.retryAWSAction (retryWithBackoff r)
