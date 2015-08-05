{-# LANGUAGE NoImplicitPrelude #-}
module Mismi.S3.Aws.Commands (
    exists
  , delete
  , read
  , headObject
  , download
  , downloadWithMode
  , multipartDownload
  , uploadCheck
  , uploadSingle
  , multipartUpload
  , calculateChunks
  , write
  , writeWithMode
  , copy
  , move
  , getObjects
  , listObjects
  , list
  , sync
  , getObjectsRecursively
  , listRecursively
  , getSize
  ) where

import           Control.Lens

import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import           Data.Text (Text)

import           Mismi.S3.Aws.Data
import           Mismi.S3.Aws.Control
import qualified Mismi.S3.Commands as A
import           Mismi.S3.Data
import           Mismi.S3.Internal

import           Network.AWS.Data (toText)

import           P

import           System.IO


exists :: Address -> S3Action Bool
exists = A.exists

headObject :: Address -> S3Action (Maybe ObjectMetadata)
headObject a = A.headObject a >>= \mr -> return . join $ (toObj <$> mr)
  where
    toObj r = ObjectMetadata
      <$> r ^. A.horDeleteMarker
      <*> r ^. A.horETag
      <*> r ^. A.horLastModified
      <*> pure (r ^. A.horVersionId)
      <*> (pure . fmap (first toText) . M.toList $ r ^. A.horMetadata)
      <*> (pure . pure . T.pack . show $ r ^. A.horMissingMeta)
      <*> (pure . toEncr <$> (r ^. A.horServerSideEncryption))
    toEncr A.AES256 = AES256

getSize :: Address -> S3Action (Maybe Int)
getSize = A.getSize

delete :: Address -> S3Action ()
delete = A.delete

read :: Address -> S3Action (Maybe Text)
read = A.read


download :: Address -> FilePath -> S3Action ()
download a p = downloadWithMode Fail a p

downloadWithMode :: WriteMode -> Address -> FilePath -> S3Action ()
downloadWithMode = A.downloadWithMode

multipartDownload :: Address -> FilePath -> Int -> Integer -> Int -> S3Action ()
multipartDownload = A.multipartDownload

uploadCheck :: FilePath -> Address -> S3Action ()
uploadCheck = A.upload

uploadSingle :: FilePath -> Address -> S3Action ()
uploadSingle = A.uploadSingle

multipartUpload :: FilePath -> Address -> Integer -> Integer -> S3Action ()
multipartUpload = A.multipartUpload'

write :: Address -> Text -> S3Action ()
write = A.write

writeWithMode :: WriteMode -> Address -> Text -> S3Action ()
writeWithMode = A.writeWithMode

copy :: Address -> Address -> S3Action ()
copy a = A.copy a

move :: Address -> Address -> S3Action ()
move = A.move

-- pair of prefixs and keys
getObjects :: Address -> S3Action ([Key], [Key])
getObjects = A.getObjects

-- Pair of list of prefixes and list of keys
listObjects :: Address -> S3Action ([Address], [Address])
listObjects = A.listObjects

-- list the addresses, keys first, then prefixes
list :: Address -> S3Action [Address]
list = A.list

sync :: SyncMode -> Address -> Address -> Int -> S3Action ()
sync = A.syncWithMode

getObjectsRecursively :: Address -> S3Action [ObjectInfo]
getObjectsRecursively a = A.getObjectsRecursively a >>= return . fmap toObj
  where
    toObj r = ObjectInfo
      (r ^. A.oKey)
      (r ^. A.oLastModified)
      (r ^. A.oETag)
      (r ^. A.oSize.to fromIntegral)
      (r ^. A.oStorageClass.to toClass)
      (r ^. A.oOwner.to toUser)
    toClass A.Glacier = Glacier
    toClass A.ReducedRedundancy = ReducedRedundancy
    toClass A.Standard = Standard
    toUser Nothing = Nothing
    toUser (Just o) = UserInfo
      <$> (o ^. A.oID)
      <*> (o ^. A.oDisplayName)

listRecursively :: Address -> S3Action [Address]
listRecursively = A.listRecursively
