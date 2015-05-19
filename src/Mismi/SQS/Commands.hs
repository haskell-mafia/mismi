{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mismi.SQS.Commands (
    createQueue
  , deleteQueue
  , readMessages
  , writeMessage
  , deleteMessage
  ) where

import qualified Aws.Sqs as SQS

import           Data.Text

import           Mismi.Control
import           Mismi.SQS.Control
import           Mismi.SQS.Data

import           P


-- http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_CreateQueue.html
createQueue :: QueueName  -> Maybe Int-> SQSAction QueueUrl
createQueue q v = do
  let createQReq = SQS.CreateQueue v . unQueueName $ q
  SQS.CreateQueueResponse qUrl <-  awsRequest $ createQReq
  maybe (fail "Failed to parse aws account number from queue url.") (\x -> pure . QueueUrl $ SQS.QueueName (unQueueName q) x) (awsAccountNum qUrl)

-- http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_DeleteQueue.html
deleteQueue :: QueueUrl -> SQSAction ()
deleteQueue (QueueUrl q) =
  void . awsRequest . SQS.DeleteQueue $ q

-- http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_SendMessage.html
writeMessage :: QueueUrl -> Text -> Maybe Int -> SQSAction (MessageId)
writeMessage (QueueUrl qName) msg d = do
  let sqsSendMessage = SQS.SendMessage msg qName [] d
  SQS.SendMessageResponse _ mid _ <- awsRequest $ sqsSendMessage
  pure . MessageId $ mid

-- http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_ReceiveMessage.html
readMessages :: QueueUrl -> Maybe Int -> Maybe Int -> SQSAction [SQS.Message]
readMessages (QueueUrl qName) n w = do
  let receiveMessageReq = SQS.ReceiveMessage Nothing [] n [] qName w
  SQS.ReceiveMessageResponse r <- awsRequest $ receiveMessageReq
  pure r

-- http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_DeleteMessage.html
deleteMessage :: QueueUrl -> SQS.Message -> SQSAction ()
deleteMessage q m = do
   void . awsRequest $ (SQS.DeleteMessage (SQS.mReceiptHandle m) (unQueueUrl q))

awsAccountNum :: Text -> Maybe Text
awsAccountNum url =
  case split (== '/') url of
    (_:_:_:a:_) -> Just a
    _ -> Nothing
