{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mismi.S3.Aws.Data (
    ObjectMetadata(..)
  , ObjectInfo(..)
  , ServerSideEncryption(..)
  , StorageClass(..)
  , UserInfo(..)
  , CanonicalUserId
  , s3EndpointUsClassic
  , s3EndpointUsWest
  , s3EndpointUsWest2
  , s3EndpointEu
  , s3EndpointApSouthEast
  , s3EndpointApSouthEast2
  , s3EndpointApNorthEast
  ) where

import           Data.Text
import           Data.Time
import qualified Data.ByteString          as B

import           P


data ObjectMetadata = ObjectMetadata {
    omDeleteMarker         :: Bool
  , omETag                 :: Text
  , omLastModified         :: UTCTime
  , omVersionId            :: Maybe Text
  , omUserMetadata         :: [(Text, Text)]
  , omMissingUserMetadata  :: Maybe Text
  , omServerSideEncryption :: Maybe ServerSideEncryption
  } deriving (Show)

data ObjectInfo = ObjectInfo {
    objectKey          :: Text
  , objectLastModified :: UTCTime
  , objectETag         :: Text
  , objectSize         :: Integer
  , objectStorageClass :: StorageClass
  , objectOwner        :: Maybe UserInfo
  } deriving (Show)


data ServerSideEncryption =
    AES256
  deriving (Show)

data StorageClass =
    Standard
  | ReducedRedundancy
  | Glacier
  deriving (Show)

data UserInfo = UserInfo {
    userId          :: CanonicalUserId
  , userDisplayName :: Text
  } deriving (Show)

type CanonicalUserId = Text


s3EndpointUsClassic :: B.ByteString
s3EndpointUsClassic = "s3.amazonaws.com"

s3EndpointUsWest :: B.ByteString
s3EndpointUsWest = "s3-us-west-1.amazonaws.com"

s3EndpointUsWest2 :: B.ByteString
s3EndpointUsWest2 = "s3-us-west-2.amazonaws.com"

s3EndpointEu :: B.ByteString
s3EndpointEu = "s3-eu-west-1.amazonaws.com"

s3EndpointApSouthEast :: B.ByteString
s3EndpointApSouthEast = "s3-ap-southeast-1.amazonaws.com"

s3EndpointApSouthEast2 :: B.ByteString
s3EndpointApSouthEast2 = "s3-ap-southeast-2.amazonaws.com"

s3EndpointApNorthEast :: B.ByteString
s3EndpointApNorthEast = "s3-ap-northeast-1.amazonaws.com"
