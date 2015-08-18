{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Mismi.SQS.Commands (
    module A
  , onQueue
  , createQueue
  , deleteQueue
  , readMessages
  , writeMessage
  , deleteMessage
  ) where

import           Control.Lens

import           Data.Text as T
import qualified Data.HashMap.Strict as M

import           Mismi
import           Mismi.SQS.Data

import           Network.AWS.SQS as A hiding (createQueue, deleteQueue, deleteMessage)
import qualified Network.AWS.SQS as A

import           P


-- | Create a queue, which may be in a different region than our global/current one (which will be ignored)
onQueue :: Queue -> Maybe Int -> (QueueUrl -> AWS a) -> AWS a
onQueue (Queue q r) v action =
  within r (action =<< createQueue q v)

-- http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_CreateQueue.html
createQueue :: QueueName -> Maybe Int -> AWS QueueUrl
createQueue q v = do
  res <- send $ A.createQueue (unQueueName q) &
           cqAttributes .~
             (M.fromList . maybeToList
                $ (("VisibilityTimeout",) <$> ((T.pack . show) <$> v)))
  maybe
    (fail $ "Failed to create new queue: " <> show q)
    (pure . QueueUrl)
    (res ^. cqrQueueUrl)

-- http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_DeleteQueue.html
deleteQueue :: QueueUrl -> AWS ()
deleteQueue =
  send_ . A.deleteQueue . unQueueUrl

-- http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_SendMessage.html
writeMessage :: QueueUrl -> Text -> Maybe Int -> AWS (MessageId)
writeMessage q m d = do
  res <- send $ A.sendMessage (unQueueUrl q) m & smDelaySeconds .~ d
  maybe
    (fail "Failed to parse MessageId")
    (pure . MessageId)
    (res ^. smrMessageId)

-- http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_ReceiveMessage.html
readMessages :: QueueUrl -> Maybe Int -> Maybe Int -> AWS [A.Message]
readMessages q n w = do
  res <- send $ A.receiveMessage (unQueueUrl q) &
           rmMaxNumberOfMessages .~ n &
           rmWaitTimeSeconds .~ w
  pure $ res ^. rmrMessages

-- http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_DeleteMessage.html
deleteMessage :: QueueUrl -> A.Message -> AWS ()
deleteMessage q m = do
   i <- maybe (fail "MessageId cannot be Nothing") pure (m ^. mReceiptHandle)
   send_ $ A.deleteMessage (unQueueUrl q) i

