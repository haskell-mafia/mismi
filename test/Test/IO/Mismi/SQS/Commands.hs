{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Mismi.SQS.Commands where

import qualified Aws.Sqs as SQS

import           Disorder.Core.IO
import           Data.Maybe

import           Mismi.SQS

import           P

import           System.IO

import           Test.Mismi.Arbitrary ()
import           Test.Mismi.SQS
import           Test.QuickCheck

prop_write_read :: Queue -> NonEmptyMessage -> Property
prop_write_read queue' msg =
  testIO . runSQSWithQueue' queue' $ \q -> do
    _ <- writeMessage q (unMessage msg) Nothing
    msg2 <- readMessages q (Just 1) Nothing
    pure $ [unMessage msg] === fmap SQS.mBody msg2

prop_write_delete_read :: Queue -> NonEmptyMessage -> Property
prop_write_delete_read queue' msg =
  testIO . runSQSWithQueue' queue' $ \q -> do
    _ <- writeMessage q (unMessage msg) Nothing
    msg2 <- readMessages q (Just 1) Nothing
    forM_ msg2 (deleteMessage q)
    msg3 <- readMessages q (Just 1) Nothing
    pure $ [] === msg3

prop_with_queue :: Queue -> NonEmptyMessage -> Property
prop_with_queue queue' msg =
  testIO . runSQSWithQueue' queue' $ \q1 -> onQueue queue' Nothing $ \q -> do
    _ <- writeMessage q (unMessage msg) Nothing
    msg2 <- readMessages q (Just 1) Nothing
    pure $ ([unMessage msg], q1)  === (fmap SQS.mBody msg2, q)

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 2 })
