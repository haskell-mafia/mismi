{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Mismi.SQS.Data (
    QueueName(..)
  , module M
  ) where

import           Aws.Sqs.Commands.Message as M

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
