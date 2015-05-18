{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Mismi.SQS.Data (
    QueueName(..)
  , QueueUrl(..)
  , MessageId(..)
  , module M
  , sqsEndpointApSouthEast2
  , sqsEndpointEuWest1
  , sqsEndpointEuCentral1
  , sqsEndpointSaEast1
  ) where

import qualified Aws.Sqs as SQS
import           Aws.Sqs.Commands.Message as M
import qualified Aws.Sqs.Core
import qualified Aws.S3.Core

import qualified Data.ByteString.Char8 as BS

import           Data.Text

import           P

-- Queue names are limited to 80 characters. Alphanumeric characters
-- plus hyphens (-) and underscores (_) are allowed. Queue names must be unique
-- within an AWS account. After you delete a queue, you can reuse the queue
-- name.
--
newtype QueueName = QueueName {
    unQueueName :: Text
  } deriving (Eq, Show)

newtype QueueUrl = QueueUrl { unQueueUrl :: SQS.QueueName } deriving (Eq, Show)
newtype MessageId = MessageId SQS.MessageId deriving (Eq, Show)

sqsEndpointApSouthEast2 :: SQS.Endpoint
sqsEndpointApSouthEast2 =
  SQS.Endpoint (BS.pack "sqs.ap-southeast-2.amazonaws.com") Aws.S3.Core.locationApSouthEast2 [Aws.S3.Core.locationApSouthEast2]

sqsEndpointEuWest1 :: SQS.Endpoint
sqsEndpointEuWest1 =
  SQS.Endpoint (BS.pack "sqs.eu-west-1.amazonaws.com") Aws.S3.Core.locationEu [Aws.S3.Core.locationEu]

sqsEndpointEuCentral1 :: SQS.Endpoint
sqsEndpointEuCentral1 =
  SQS.Endpoint (BS.pack "sqs.eu-central-1.amazonaws.com") Aws.S3.Core.locationEuFrankfurt [Aws.S3.Core.locationEuFrankfurt]

sqsEndpointSaEast1 :: SQS.Endpoint
sqsEndpointSaEast1 =
  SQS.Endpoint (BS.pack "sqs.sa-east-1.amazonaws.com") Aws.S3.Core.locationSA [Aws.S3.Core.locationSA]
