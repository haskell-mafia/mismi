{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mismi.SQS.Core.Data (
    QueueName (..)
  , Queue (..)
  , QueueUrl (..)
  , MessageId (..)
  ) where

import           Mismi.Kernel.Data (MismiRegion)

import           P


-- Queue names are limited to 80 characters. Alphanumeric characters
-- plus hyphens (-) and underscores (_) are allowed. Queue names must
-- be unique within an AWS account. After you delete a queue, you can
-- reuse the queue name.
newtype QueueName =
  QueueName {
      renderQueueName :: Text
    } deriving (Eq, Show)

data Queue =
  Queue {
      queueName :: QueueName
    , queueRegion :: MismiRegion
    } deriving (Eq, Show)

newtype QueueUrl =
  QueueUrl {
      renderQueueUrl :: Text
    } deriving (Eq, Show)

newtype MessageId =
  MessageId {
      renderMessageId :: Text
    } deriving (Eq, Show)
