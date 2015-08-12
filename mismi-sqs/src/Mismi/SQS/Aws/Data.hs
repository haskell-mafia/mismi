{-# LANGUAGE NoImplicitPrelude #-}
module Mismi.SQS.Aws.Data (
    Endpoint(..)
  , Message(..)
  , ReceiptHandle(..)
  , MessageAttribute(..)
  , UserMessageAttribute
  , UserMessageAttributeValue(..)
  ) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import           Data.Scientific

import           P
import           Prelude (Bounded, Enum)


data Endpoint = Endpoint {
    endpointHost :: B.ByteString
  , endpointDefaultLocationConstraint :: LocationConstraint
  , endpointAllowedLocationConstraints :: [LocationConstraint]
  } deriving (Show)

type LocationConstraint = T.Text

data Message = Message {
    mMessageId :: !T.Text
    -- ^ A unique identifier for the message. Message IDs are considered unique
    -- across all AWS accounts for an extended period of time.

  , mReceiptHandle :: !ReceiptHandle
    -- ^ An identifier associated with the act of receiving the message. A new
    -- receipt handle is returned every time you receive a message. When deleting
    -- a message, you provide the last received receipt handle to delete the
    -- message.

  , mMD5OfBody :: !T.Text
    -- ^ An MD5 digest of the non-URL-encoded message body string.

  , mBody :: T.Text
    -- ^ The message's contents (not URL-encoded).

  , mAttributes :: ![(MessageAttribute,T.Text)]
    -- ^ SenderId, SentTimestamp, ApproximateReceiveCount, and/or
    -- ApproximateFirstReceiveTimestamp. SentTimestamp and
    -- ApproximateFirstReceiveTimestamp are each returned as an integer
    -- representing the epoch time in milliseconds.

  , mMD5OfMessageAttributes :: !(Maybe T.Text)
    -- ^ An MD5 digest of the non-URL-encoded message attribute string. This can
    -- be used to verify that Amazon SQS received the message correctly. Amazon
    -- SQS first URL decodes the message before creating the MD5 digest. For
    -- information about MD5, go to <http://www.faqs.org/rfcs/rfc1321.html>.

  , mUserMessageAttributes :: ![UserMessageAttribute]
    -- ^ Each message attribute consists of a Name, Type, and Value.
   } deriving(Show, Read, Eq, Ord)

newtype ReceiptHandle = ReceiptHandle {
    unReceiptHandle :: T.Text
  } deriving(Show, Read, Eq, Ord)

data MessageAttribute =
    MessageAll
    -- ^ all values
  | SenderId
    -- ^ the AWS account number (or the IP address, if anonymous access is
    -- allowed) of the sender
  | SentTimestamp
    -- ^ the time when the message was sent (epoch time in milliseconds)
  | ApproximateReceiveCount
    -- ^ the number of times a message has been received but not deleted
  | ApproximateFirstReceiveTimestamp
    -- ^ the time when the message was first received (epoch time in
    -- milliseconds)
  deriving(Show,Read,Eq,Ord,Enum,Bounded)

type UserMessageAttribute = (UserMessageAttributeName, UserMessageAttributeValue)

type UserMessageAttributeName = T.Text

data UserMessageAttributeValue =
    UserMessageAttributeString (Maybe UserMessageAttributeCustomType) T.Text
    -- ^ Strings are Unicode with UTF-8 binary encoding.

  | UserMessageAttributeNumber (Maybe UserMessageAttributeCustomType) Scientific
    -- ^ Numbers are positive or negative integers or floating point numbers.
    -- Numbers have sufficient range and precision to encompass most of the
    -- possible values that integers, floats, and doubles typically support. A
    -- number can have up to 38 digits of precision, and it can be between
    -- 10^-128 to 10^+126. Leading and trailing zeroes are trimmed.

  | UserMessageAttributeBinary (Maybe UserMessageAttributeCustomType) B.ByteString
    -- ^ Binary type attributes can store any binary data, for example,
    -- compressed data, encrypted data, or images.

    -- UserMessageAttributesStringList (Maybe UserMessageAttributeCustomType) [T.Text]
    -- -- ^ Not implemented. Reserved for future use.

    -- UserMessageAttributeBinaryList (Maybe UserMessageAttributeCustomType) [B.ByteString]
    -- -- ^ Not implemented. Reserved for future use.
  deriving (Show, Read, Eq, Ord)

type UserMessageAttributeCustomType = T.Text
