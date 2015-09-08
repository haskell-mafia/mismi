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
  , uploadWithMode
  , write
  , writeWithMode
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
import           Mismi.S3.Internal

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

upload :: FilePath -> Address -> AWS ()
upload f = retryAction 3 . A.upload f

uploadWithMode :: WriteMode -> FilePath -> Address -> AWS ()
uploadWithMode m f = retryAction 3 . A.uploadWithMode m f

write :: Address -> Text -> AWS ()
write a = retryAction 5 . A.write a

writeWithMode :: WriteMode -> Address -> Text -> AWS ()
writeWithMode m a = retryAction 5 . A.writeWithMode m a

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
retryAction r = retryAWSAction (retryWithBackoff r)
