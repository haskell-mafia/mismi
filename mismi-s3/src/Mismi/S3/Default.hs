{-# LANGUAGE NoImplicitPrelude #-}
{- |
An exposed set of S3 operations that have the default retries/backoff for batch operations.
See "Mismi.S3.Commands" for the raw operations that use the default, global retry.
-}
module Mismi.S3.Default (
    module X
  , exists
  , headObject
  , getSize
  , delete
  , read
  , download
  , downloadWithMode
  , multipartDownload
  , upload
  , uploadOrFail
  , uploadWithMode
  , uploadWithModeOrFail
  , write
  , writeOrFail
  , writeWithMode
  , writeWithModeOrFail
  , copy
  , copyWithMode
  , move
  , listObjects
  , list
  , list'
  , listMultiparts
  , listRecursively
  , listRecursively'
  , getObjectsRecursively
  , abortMultipart
  , sync
  , syncWithMode
  ) where

import           Data.Text hiding (copy)

import           Data.Conduit

import           Mismi.S3.Commands as X (AWS, MultipartUpload, muUploadId, retryAWSAction)
import qualified Mismi.S3.Commands as A
import           Mismi.S3.Data

import           P

import           System.IO (FilePath)


headObject :: Address -> AWS (Maybe A.HeadObjectResponse)
headObject = retryAction 5 . A.headObject

exists :: Address -> AWS Bool
exists = retryAction 5 . A.exists

getSize :: Address -> AWS (Maybe Int)
getSize = retryAction 5 . A.getSize

delete :: Address -> AWS ()
delete = retryAction 5 . A.delete

read :: Address -> AWS (Maybe Text)
read = retryAction 5 . A.read

copy :: Address -> Address -> AWS ()
copy s = retryAction 5 . A.copy s

copyWithMode :: WriteMode -> Address -> Address -> AWS ()
copyWithMode m s = retryAction 5 . A.copyWithMode m s

move :: Address -> Address -> AWS ()
move s = retryAction 5 . A.move s

listObjects :: Address -> AWS ([Address], [Address])
listObjects = retryAction 5 . A.listObjects

list :: Address -> AWS [Address]
list = retryAction 5 . A.list

list' :: Address -> AWS (Source AWS Address)
list' = retryAction 5 . A.list'

listMultiparts :: Bucket -> AWS [MultipartUpload]
listMultiparts = retryAction 5 . A.listMultiparts

getObjectsRecursively :: Address -> AWS [A.Object]
getObjectsRecursively = retryAction 5 . A.getObjectsRecursively

abortMultipart :: Bucket -> MultipartUpload -> AWS ()
abortMultipart b = retryAction 5 . A.abortMultipart b

upload :: FilePath -> Address -> AWS UploadResult
upload f = retryAction 3 . A.upload f

uploadOrFail :: FilePath -> Address -> AWS ()
uploadOrFail f = retryAction 3 . A.uploadOrFail f

uploadWithMode :: WriteMode -> FilePath -> Address -> AWS UploadResult
uploadWithMode m f = retryAction 3 . A.uploadWithMode m f

uploadWithModeOrFail :: WriteMode -> FilePath -> Address -> AWS ()
uploadWithModeOrFail m f = retryAction 3 . A.uploadWithModeOrFail m f

write :: Address -> Text -> AWS WriteResult
write a = retryAction 5 . A.write a

writeOrFail :: Address -> Text -> AWS ()
writeOrFail a = retryAction 5 . A.writeOrFail a

writeWithMode :: WriteMode -> Address -> Text -> AWS WriteResult
writeWithMode m a = retryAction 5 . A.writeWithMode m a

writeWithModeOrFail :: WriteMode -> Address -> Text -> AWS ()
writeWithModeOrFail m a = retryAction 5 . A.writeWithModeOrFail m a

download :: Address -> FilePath -> AWS ()
download a = retryAction 5 . A.download a

downloadWithMode :: WriteMode -> Address -> FilePath -> AWS ()
downloadWithMode m a = retryAction 5 . A.downloadWithMode m a

multipartDownload :: Address -> FilePath -> Int -> Integer -> Int -> AWS ()
multipartDownload a s d sz = retryAction 3 . A.multipartDownload a s d sz

listRecursively :: Address -> AWS [Address]
listRecursively = retryAction 5 . A.listRecursively

listRecursively' :: Address -> AWS (Source AWS Address)
listRecursively' = retryAction 5 . A.listRecursively'


sync :: Address -> Address -> Int -> AWS ()
sync s d = retryAction 3 . A.sync s d

syncWithMode :: SyncMode -> Address -> Address -> Int -> AWS ()
syncWithMode m s d = retryAction 3 . A.syncWithMode m s d

retryAction :: Int -> AWS a -> AWS a
retryAction = retryAWSAction
