{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TupleSections     #-}
module Mismi.SQS.Aws.Commands (
    onQueue
  , createQueue
  , deleteQueue
  , readMessages
  , writeMessage
  , deleteMessage
  ) where

import           Control.Lens

import           Data.Text as T
import qualified Data.HashMap.Strict as M
import           Data.Scientific

import           Mismi.SQS.Aws.Control
import           Mismi.SQS.Aws.Data
import           Mismi.SQS.Data

import qualified Mismi.SQS.Data as A
import qualified Mismi.SQS.Commands as A

import qualified Network.AWS.Data as A

import           P


-- | Create a queue, which may be in a different region than our global/current one (which will be ignored)
onQueue :: Queue -> Maybe Int -> (QueueUrl -> SQSAction a) -> SQSAction a
onQueue = A.onQueue

-- http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_CreateQueue.html
createQueue :: QueueName -> Maybe Int -> SQSAction QueueUrl
createQueue (QueueName n) = A.createQueue (A.QueueName n)

-- http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_DeleteQueue.html
deleteQueue :: QueueUrl -> SQSAction ()
deleteQueue = A.deleteQueue

-- http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_SendMessage.html
writeMessage :: QueueUrl -> Text -> Maybe Int -> SQSAction (MessageId)
writeMessage q m d = A.writeMessage q m d

-- http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_ReceiveMessage.html
readMessages :: QueueUrl -> Maybe Int -> Maybe Int -> SQSAction [Message]
readMessages q n w = do
  ms <- A.readMessages q n w
  traverse toMessage ms

-- http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_DeleteMessage.html
deleteMessage :: QueueUrl -> Message -> SQSAction ()
deleteMessage q Message { mMessageId = i, mReceiptHandle = h } =
  A.deleteMessage q $ A.message
    & A.mMessageId .~ pure i
    & A.mReceiptHandle .~ pure (unReceiptHandle h)

toMessage :: A.Message -> SQSAction Message
toMessage m = Message
  <$> (parseMaybe "MessageId" $ m ^. A.mMessageId)
  <*> (ReceiptHandle <$> parseMaybe "ReceiptHandle" (m ^. A.mReceiptHandle))
  <*> (parseMaybe "MD5OfBody" $ m ^. A.mMD5OfBody)
  <*> (parseMaybe "Body" $ m ^. A.mBody)
  <*> (toAttributes $ m ^. A.mAttributes)
  <*> (pure $ m ^. A.mMD5OfMessageAttributes)
  <*> (toUserMessageAttributes $ m ^. A.mMessageAttributes)


toUserMessageAttributes :: M.HashMap Text A.MessageAttributeValue -> SQSAction [UserMessageAttribute]
toUserMessageAttributes = traverse toPair . M.toList
  where toPair (k, v) = (k,) <$> toUserMessageAttributeValue v

toUserMessageAttributeValue :: A.MessageAttributeValue -> SQSAction UserMessageAttributeValue
toUserMessageAttributeValue v = do
  let (t, c) = case breakOn "." $ v ^. A.mavDataType of
        (t', "") -> (t', Nothing)
        (t', c') -> (t', Just (tail c'))
  case t of
    "String" -> UserMessageAttributeString c
      <$> parseMaybe "UserMessageAttributeString value" (v ^. A.mavStringValue)
    "Number" -> UserMessageAttributeNumber c
      <$> (toScientific =<< parseMaybe "UserMessageAttributeNumber value" (v ^. A.mavStringValue))
    "Binary" -> UserMessageAttributeBinary c
      <$> (A.toBS <$> parseMaybe "UserMessageAttributeBinary value" (v ^. A.mavBinaryValue))
    u -> fail $ "Unsupported MessageAttributeValue type: " <> unpack u
  where
    toScientific :: Text -> SQSAction Scientific
    toScientific = either fail pure . readEither . unpack

toMessageAttribute :: Text -> SQSAction MessageAttribute
toMessageAttribute = \case
  "All" -> pure MessageAll
  "ApproximateFirstReceiveTimestamp" -> pure ApproximateFirstReceiveTimestamp
  "ApproximateReceiveCount" -> pure ApproximateReceiveCount
  "SenderId" -> pure SenderId
  "SentTimestamp" -> pure SentTimestamp
  u -> fail $ "Unsupported MessageAttribute: " <> unpack u

toAttributes :: M.HashMap A.QueueAttributeName Text -> SQSAction [(MessageAttribute, T.Text)]
toAttributes = traverse toPair . M.toList
  where toPair (a, v) = (,) <$> toMessageAttribute (A.toText a) <*> pure v

parseMaybe :: Text -> Maybe a -> SQSAction a
parseMaybe f = maybe (fail $ "Required field " <> unpack f <> "is missing in response") pure
