{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mismi.S3.Aws.Data (
    ObjectMetadata(..)
  , ObjectInfo(..)
  , ServerSideEncryption(..)
  , StorageClass(..)
  , UserInfo(..)
  , CanonicalUserId
  , TimeInfo(..)
  , Credentials(..)
  , V4Key
  , Logger
  , LogLevel(..)
  , s3EndpointUsClassic
  , s3EndpointUsWest
  , s3EndpointUsWest2
  , s3EndpointEu
  , s3EndpointApSouthEast
  , s3EndpointApSouthEast2
  , s3EndpointApNorthEast
  ) where

import           Data.IORef
import           Data.Text
import           Data.Time
import qualified Data.ByteString          as B

import           P

import           System.IO


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


-- | Whether to restrict the signature validity with a plain timestamp, or with explicit expiration
-- (absolute or relative).
data TimeInfo =
    Timestamp                                      -- ^ Use a simple timestamp to let AWS check the request validity.
  | ExpiresAt { fromExpiresAt :: UTCTime }         -- ^ Let requests expire at a specific fixed time.
  | ExpiresIn { fromExpiresIn :: NominalDiffTime } -- ^ Let requests expire a specific number of seconds after they
                                                     -- were generated.
   deriving (Show)


-- | AWS access credentials.
data Credentials
    = Credentials {
        -- | AWS Access Key ID.
        accessKeyID :: B.ByteString
        -- | AWS Secret Access Key.
      , secretAccessKey :: B.ByteString
        -- | Signing keys for signature version 4
      , v4SigningKeys :: IORef [V4Key]
        -- | Signed IAM token
      , iamToken :: Maybe B.ByteString
      }

-- | Signature version 4: ((region, service),(date,key))
type V4Key = ((B.ByteString,B.ByteString),(B.ByteString,B.ByteString))

-- | The interface for any logging function. Takes log level and a log message, and can perform an arbitrary
-- IO action.
type Logger = LogLevel -> Text -> IO ()

-- | The severity of a log message, in rising order.
data LogLevel =
    Debug
  | Info
  | Warning
  | Error
  deriving (Show, Eq, Ord)


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
