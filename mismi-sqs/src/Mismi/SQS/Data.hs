{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Mismi.SQS.Data (
    QueueName(..)
  , Queue(..)
  , QueueUrl(..)
  , MessageId(..)
  ) where

import           Data.Text
import           Network.AWS.Types
import           P

-- Queue names are limited to 80 characters. Alphanumeric characters
-- plus hyphens (-) and underscores (_) are allowed. Queue names must be unique
-- within an AWS account. After you delete a queue, you can reuse the queue
-- name.
newtype QueueName = QueueName {
    unQueueName :: Text
  } deriving (Eq, Show)

data Queue = Queue {
    queueName :: QueueName
  , queueRegion :: Region
  } deriving (Eq, Show)

newtype QueueUrl = QueueUrl {
    unQueueUrl :: Text
  } deriving (Eq, Show)

newtype MessageId = MessageId {
    unMessageId :: Text
  } deriving (Eq, Show)
