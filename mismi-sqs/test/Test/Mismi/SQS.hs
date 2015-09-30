{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Test.Mismi.SQS (
    module X
  , NonEmptyMessage(..)
  , withQueue
  , withQueueArg
  , runSQSWithQueue
  , runSQSWithQueueArg
  ) where

import           Data.Text

import           Mismi.SQS as M
import qualified Mismi.Amazonka as M

import           Test.Mismi as X
import           Test.Mismi.SQS.Arbitrary ()
import           Test.QuickCheck

import           P

import           System.IO

data NonEmptyMessage = NonEmptyMessage {
    unMessage :: Text
  } deriving (Eq, Show)

instance Arbitrary NonEmptyMessage where
  arbitrary = do
    -- http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_SendMessage.html
    -- invalid unicode values #x9 | #xA | #xD | [#x20 to #xD7FF] | [#xE000 to #xFFFD] | [#x10000 to #x10FFFF]
    NonEmptyMessage <$> genSQSText

genSQSText :: Gen Text
genSQSText =
           let invalid = P.concat [['\x9'],['\xA'],['\xD'], ['\x20'..'\xD7FF'], ['\xE000'..'\xFFFD'], ['\x10000'..'\x10FFFF']]
           in suchThat (pack . P.filter (\x -> P.elem x invalid) <$> arbitrary) (not . Data.Text.null)

withQueue :: QueueName -> (QueueUrl -> AWS a) -> AWS a
withQueue = withQueueArg testVisibilityTimeout

withQueueArg :: Maybe Int -> QueueName -> (QueueUrl -> AWS a) -> AWS a
withQueueArg v q f =
  awsBracket (createQueue q v) (void . deleteQueue) f


runSQSWithQueue :: Queue -> (QueueUrl -> AWS a) -> IO a
runSQSWithQueue = runSQSWithQueueArg testVisibilityTimeout

runSQSWithQueueArg :: Maybe Int -> Queue -> (QueueUrl -> AWS a) -> IO a
runSQSWithQueueArg v (Queue qn r) f = do
  runAWSDefaultRegion . M.within r $ withQueueArg v qn f

testVisibilityTimeout :: Maybe Int
testVisibilityTimeout = Just 8400
