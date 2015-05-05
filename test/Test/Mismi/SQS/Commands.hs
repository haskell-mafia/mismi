{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Mismi.SQS.Commands where

import qualified Aws.Sqs as SQS

import           Mismi.SQS

import           Orphanarium.Core.IO

import           P

import           System.IO

import           Test.Mismi.Arbitrary ()
import           Test.Mismi.SQS
import           Test.QuickCheck

prop_write_read :: QueueName -> NonEmptyMessage -> Property
prop_write_read queueName msg =
  testIO $ do
    withQueue' queueName $ \q -> do
      _ <- writeMessage q (unMessage msg) Nothing
      msg2 <- readMessages q (Just 1) Nothing
      pure $ [unMessage msg] === fmap SQS.mBody msg2

prop_write_delete_read :: QueueName -> NonEmptyMessage -> Property
prop_write_delete_read queueName msg =
  testIO $ do
    withQueue' queueName $ \q -> do
      _ <- writeMessage q (unMessage msg) Nothing
      msg2 <- readMessages q (Just 1) Nothing
      forM_ msg2 (deleteMessage q)
      msg3 <- readMessages q (Just 1) Nothing
      pure $ [] === msg3

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 2 })
