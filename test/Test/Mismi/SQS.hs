{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Mismi.SQS (
    NonEmptyMessage(..)
  , withQueue'
  ) where

import           Data.Text

import           Mismi.SQS

import           Test.QuickCheck

import           P

import           System.IO

data NonEmptyMessage = NonEmptyMessage { unMessage :: Text } deriving (Eq,Show)

instance Arbitrary NonEmptyMessage where
  arbitrary = do
    -- http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_SendMessage.html
    -- invalid unicode values #x9 | #xA | #xD | [#x20 to #xD7FF] | [#xE000 to #xFFFD] | [#x10000 to #x10FFFF]
    NonEmptyMessage <$> genSQSText

genSQSText :: Gen Text
genSQSText =
           let invalid = P.concat [['\x9'],['\xA'],['\xD'], ['\x20'..'\xD7FF'], ['\xE000'..'\xFFFD'], ['\x10000'..'\x10FFFF']]
           in suchThat (pack . P.filter (\x -> P.elem x invalid) <$> arbitrary) (not . Data.Text.null)

withQueue' :: QueueName -> (QueueUrl -> SQSAction a) -> IO a
withQueue' q f =
     withQueue q $ \qUrl -> do
       a <- f qUrl
       _ <- deleteQueue qUrl
       pure a
