{-# LANGUAGE NoImplicitPrelude #-}
{- |
An exposed set of S3 operations that have the default retries/backoff for batch operations.
See "Mismi.S3.Amazonka" for the raw operations that use the default, global retry.
-}
module Mismi.S3.DefaultK (
    module X
  , headObject
  , exists
  , getSize
  , delete
  , copy
  , copyWithMode
  , upload
  , download
  , listMultipartParts
  , listMultiparts
  , listOldMultiparts
  , abortMultipart
  , listRecursively
  , sync
  , syncWithMode
  ) where

import           Data.Text hiding (copy)

import           Mismi.S3.Amazonka as X (AWS, MultipartUpload, muUploadId, retryAWSAction)
import qualified Mismi.S3.Amazonka as A
import           Mismi.S3.Data
import           Mismi.S3.Internal

import           P

import           System.IO (FilePath)


headObject :: Address -> AWS (Maybe A.HeadObjectResponse)
headObject = retryAWSAction (retryWithBackoff 5) . A.headObject

exists :: Address -> AWS Bool
exists = retryAWSAction (retryWithBackoff 5) . A.exists

getSize :: Address -> AWS (Maybe Int)
getSize = retryAWSAction (retryWithBackoff 5) . A.getSize

delete :: Address -> AWS ()
delete = retryAWSAction (retryWithBackoff 5) . A.delete

copy :: Address -> Address -> AWS ()
copy s = retryAWSAction (retryWithBackoff 5) . A.copy s

copyWithMode :: WriteMode -> Address -> Address -> AWS ()
copyWithMode wm s = retryAWSAction (retryWithBackoff 5) . A.copyWithMode wm s

upload :: FilePath -> Address -> AWS ()
upload f = retryAWSAction (retryWithBackoff 5) . A.upload f

download :: Address -> FilePath -> AWS ()
download a = retryAWSAction (retryWithBackoff 5) . A.download a

listMultipartParts :: Address -> Text -> AWS [A.Part]
listMultipartParts a = retryAWSAction (retryWithBackoff 5) . A.listMultipartParts a

listMultiparts :: Bucket -> AWS [A.MultipartUpload]
listMultiparts = retryAWSAction (retryWithBackoff 5) . A.listMultiparts

listOldMultiparts :: Bucket -> AWS [A.MultipartUpload]
listOldMultiparts = retryAWSAction (retryWithBackoff 5) . A.listOldMultiparts

abortMultipart :: Bucket -> A.MultipartUpload -> AWS ()
abortMultipart b = retryAWSAction (retryWithBackoff 5) . A.abortMultipart b

listRecursively :: Address -> AWS [Address]
listRecursively = retryAWSAction (retryWithBackoff 5) . A.listRecursively

sync :: Address -> Address -> Int -> AWS ()
sync s d = retryAWSAction (retryWithBackoff 5) . A.sync s d

syncWithMode :: SyncMode -> Address -> Address -> Int -> AWS ()
syncWithMode sm s d = retryAWSAction (retryWithBackoff 5) . A.syncWithMode sm s d
