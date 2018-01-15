{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Mismi.SQS.Commands where

import           Control.Lens
import           Data.Maybe
import           Disorder.Core.IO

import           Mismi.SQS
import           Mismi.SQS.Amazonka hiding (createQueue, deleteMessage)

import           P

import           System.IO

import           Test.Mismi.SQS
import           Test.QuickCheck


prop_write_read :: Queue -> NonEmptyMessage -> Property
prop_write_read queue' (NonEmptyMessage b) =
  testIO . runSQSWithQueue queue' $ \q -> do
    void $ writeMessage q b Nothing
    ms <- readMessages q (Just 1) Nothing
    pure $ [Just b] === fmap (^. mBody) ms

prop_write_read_twice_visible :: Queue -> NonEmptyMessage -> Property
prop_write_read_twice_visible queue' (NonEmptyMessage b) =
  testIO . runSQSWithQueueArg (Just 0) queue' $ \q -> do
    void $ writeMessage q b Nothing
    ms1 <- readMessages q (Just 1) Nothing
    ms2 <- readMessages q (Just 1) Nothing
    pure $
      [Just b] === fmap (^. mBody) ms1 .&&.
      [Just b] === fmap (^. mBody) ms2

prop_write_read_twice_hidden :: Queue -> NonEmptyMessage -> Property
prop_write_read_twice_hidden queue' (NonEmptyMessage b) =
  testIO . runSQSWithQueue queue' $ \q -> do
    void $ writeMessage q b Nothing
    ms1 <- readMessages q (Just 1) Nothing
    ms2 <- readMessages q (Just 1) Nothing
    pure $
      [Just b] === fmap (^. mBody) ms1 .&&.
      [] === fmap (^. mBody) ms2

prop_write_delete_read :: Queue -> NonEmptyMessage -> Property
prop_write_delete_read queue' (NonEmptyMessage b) =
  testIO . runSQSWithQueue queue' $ \q -> do
    void $ writeMessage q b Nothing
    ms <- readMessages q (Just 1) Nothing
    forM_ ms (deleteMessage q)
    ms' <- readMessages q (Just 1) Nothing
    pure $ [] === ms'

prop_with_queue :: Queue -> NonEmptyMessage -> Property
prop_with_queue queue' (NonEmptyMessage b) =
  testIO . runSQSWithQueue queue' $ \q1 -> onQueue queue' Nothing $ \q -> do
    void $ writeMessage q b Nothing
    ms <- readMessages q (Just 1) Nothing
    pure $ ([Just b], q1)  === (fmap (^. mBody) ms, q)

-- | Old 'ordinance' new 'sanction' situation
prop_create_upgrade :: Queue -> NonEmptyMessage -> Property
prop_create_upgrade queue' (NonEmptyMessage b) =
  -- create queue with default VisibilityTimeout
  testIO . runSQSWithQueueArg Nothing queue' $ \q -> do
    -- get existing queue with non-default VisibilityTimeout
    q' <- createQueue (queueName queue') (Just 8400)
    void $ writeMessage q b Nothing
    ms <- readMessages q' (Just 1) Nothing
    pure $ [Just b] === fmap (^. mBody) ms

prop_create_downgrade :: Queue -> NonEmptyMessage -> Property
prop_create_downgrade queue' (NonEmptyMessage b) =
  -- create queue with non-default VisibilityTimeout
  testIO . runSQSWithQueueArg (Just 8400) queue' $ \q -> do
    -- get existing queue with default VisibilityTimeout
    q' <- createQueue (queueName queue') Nothing
    void $ writeMessage q b Nothing
    ms <- readMessages q' (Just 1) Nothing
    pure $ [Just b] === fmap (^. mBody) ms


prop_create_queue :: Queue -> Property
prop_create_queue queue =
  Test.QuickCheck.once . testIO . runSQSWithQueue queue $ \_ -> do
    _ <- onQueue queue (Just 8400) $ \_ ->
      pure ()
    _ <- onQueue queue (Just 8400) $ \_ ->
      pure ()
    pure $ property True

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 2 })
