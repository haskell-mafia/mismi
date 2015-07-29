{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Mismi.S3.Retry (
    exists
  , headObject
  , getSize
  , delete
  , read
  -- FIX We're currently downloading to the target address - it needs to download to 'file.tmp1234' and then move atomically
  , M.download
  , M.downloadWithMode
  , M.multipartDownload
  , M.uploadCheck
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

import           Control.Retry

import           Data.Text hiding (copy)

import qualified Mismi.S3.Amazonka as A
import qualified Mismi.S3.Commands as M
import           Mismi.S3.Control
import           Mismi.S3.Data

import           P

import           System.IO (FilePath)


exists :: RetryPolicy -> Address -> S3Action Bool
exists r = retryHttpWithPolicy r . M.exists

headObject :: RetryPolicy -> Address -> S3Action (Maybe ObjectMetadata)
headObject r = retryHttpWithPolicy r . M.headObject

getSize :: RetryPolicy -> Address -> S3Action (Maybe Int)
getSize r = retryHttpWithPolicy r . M.getSize

delete :: RetryPolicy -> Address -> S3Action ()
delete r = retryHttpWithPolicy r . M.delete

read :: RetryPolicy -> Address -> S3Action (Maybe Text)
read r = retryHttpWithPolicy r . M.read

upload :: RetryPolicy -> FilePath -> Address -> S3Action ()
upload r file a =
  retryHttpWithPolicy r (M.uploadCheck file a) >>= \case
    UploadSingle ->
      retryHttpWithPolicy r $ M.uploadSingle file a
    UploadMultipart fs chunk ->
      M.multipartUpload r file a fs chunk

write :: RetryPolicy -> Address -> Text -> S3Action ()
write r a = retryHttpWithPolicy r . M.write a

writeWithMode :: RetryPolicy -> WriteMode -> Address -> Text -> S3Action ()
writeWithMode r wm a = retryHttpWithPolicy r . M.writeWithMode wm a

copy :: RetryPolicy -> Address -> Address -> S3Action ()
copy r a = retryHttpWithPolicy r . liftAWSAction . A.copy a

move :: RetryPolicy -> Address -> Address -> S3Action ()
move r source destination =
  copy r source destination >>
    delete r source

listObjects :: RetryPolicy -> Address -> S3Action ([Address], [Address])
listObjects r = retryHttpWithPolicy r . M.listObjects

list :: RetryPolicy -> Address -> S3Action [Address]
list r = retryHttpWithPolicy r . M.list

getObjectsRecursively :: RetryPolicy -> Address -> S3Action [ObjectInfo]
getObjectsRecursively r = retryHttpWithPolicy r . M.getObjectsRecursively

listRecursively :: RetryPolicy -> Address -> S3Action [Address]
listRecursively r = retryHttpWithPolicy r . M.listRecursively

sync :: RetryPolicy -> SyncMode -> Address -> Address -> Int -> S3Action ()
sync rp mode source dest fork =
  liftAWSAction . A.retryAWSAction rp $ A.syncWithMode mode source dest fork
