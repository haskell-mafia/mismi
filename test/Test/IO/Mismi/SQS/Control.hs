{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Mismi.SQS.Control where

import           Mismi.SQS

import           P

import           System.IO

import           Test.Mismi.S3
import           Test.Mismi.SQS
import           Test.Mismi.Arbitrary ()
import           Test.QuickCheck

prop_result :: NonEmptyMessage -> Queue -> Property
prop_result t (Queue q r) = testAWS r $ do
  r' <- liftSQSAction . withQueue q $ \a -> do
    _ <- writeMessage a (unMessage t) Nothing
    readMessages a (Just 1) Nothing
  pure $ (P.length r') === 1

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1 })
