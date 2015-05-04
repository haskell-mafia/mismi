{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mismi.SQS.Commands (
    QueueUrl
  , MessageId
  , createQueue
  , deleteQueue
  , readMessage
  , writeMessage
  , withQueue
  ) where

import qualified Aws.Sqs as SQS

import           Data.Text

import           Mismi.Control
import           Mismi.SQS.Control
import           Mismi.SQS.Data

import           P

import           System.IO

-- http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_CreateQueue.html
defaultVisibilityTimeout :: Maybe Int
defaultVisibilityTimeout = Just 8400 -- seconds

-- http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_ReceiveMessage.html
waitTimeSeconds :: Maybe Int
waitTimeSeconds = Just 20 -- seconds

-- http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_SendMessage.html
delaySeconds :: Maybe Int
delaySeconds = Just 0 -- seconds

withQueue :: QueueName -> (QueueUrl -> SQSAction a) -> IO a
withQueue qName f =
   runSQSWithDefaults $ createQueue qName >>= f

createQueue :: QueueName -> SQSAction QueueUrl
createQueue q = do
  let createQReq = SQS.CreateQueue defaultVisibilityTimeout . unQueueName $ q
  SQS.CreateQueueResponse qUrl <-  awsRequest $ createQReq
  maybe (fail "Failed to parse aws account number from queue url.") (\x -> pure . QueueUrl $ SQS.QueueName (unQueueName q) x) (awsAccountNum qUrl)

deleteQueue :: QueueUrl -> SQSAction ()
deleteQueue (QueueUrl q) =
  void . awsRequest . SQS.DeleteQueue $ q

writeMessage :: QueueUrl -> Text -> SQSAction (MessageId)
writeMessage (QueueUrl qName) msg = do
  let sqsSendMessage = SQS.SendMessage msg qName [] delaySeconds
  SQS.SendMessageResponse _ mid _ <- awsRequest $ sqsSendMessage
  pure . MessageId $ mid

readMessage :: QueueUrl -> SQSAction (Maybe Text)
readMessage (QueueUrl qName) = do
  let receiveMessageReq = SQS.ReceiveMessage Nothing [] (Just 1) [] qName waitTimeSeconds
  SQS.ReceiveMessageResponse r <- awsRequest $ receiveMessageReq
  forM_ r (\m -> awsRequest $ (SQS.DeleteMessage (SQS.mReceiptHandle m) qName))
  pure . fmap SQS.mBody . listToMaybe $ r

awsAccountNum :: Text -> Maybe Text
awsAccountNum url =
  case split (== '/') url of
    (_:_:_:a:_) -> Just a
    _ -> Nothing
