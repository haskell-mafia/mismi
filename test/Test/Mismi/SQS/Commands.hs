{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Mismi.SQS.Commands where

import           Control.Applicative

import           Data.Maybe

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
    withQueue queueName $ \q -> do
      _ <- writeMessage q $ unMessage msg
      msg2 <- readMessage q
      pure $ (unMessage msg) === fromJust msg2

prop_write_read_read :: QueueName -> NonEmptyMessage -> Property
prop_write_read_read queueName msg =
  testIO $ do
    withQueue queueName $ \q -> do
      _ <- writeMessage q $ unMessage msg
      _ <- readMessage q
      msg3 <- readMessage q
      pure $ Nothing === msg3

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 2 })
