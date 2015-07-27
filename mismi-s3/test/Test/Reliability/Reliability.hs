{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Reliability.Reliability where

import           Control.Monad.IO.Class

import           Disorder.Core.IO

import           Mismi.S3.Control
import           Mismi.S3.Amazonka
import           Mismi.S3.Data

import           P
import qualified Prelude as P

import           System.IO
import           System.Environment

import           Test.Mismi.Amazonka
import           Test.Mismi.S3
import           Test.QuickCheck

testS3 :: Testable a => (Address -> Int -> S3Action a) -> Property
testS3 f =
  property $ \t -> testIO .
    retryHttpWithMessage 5 "Reliability" . runS3WithDefaults . withToken t $ \a -> do
      i <- liftIO $ testSize
      f a i

testAWS :: Testable a => (Address -> Int -> AWS a) -> Property
testAWS f =
  property $ \t -> testIO .
    unsafeAWS . runAWS Sydney . withAWSToken t $ \a -> do
      i <- liftIO $ testSize
      f a i

testAWS' :: Testable a => (Address -> Address -> Int -> AWS a) -> Property
testAWS' f =
  property $ \t t' -> testIO .
    unsafeAWS . runAWS Sydney . withAWSToken t $ \a ->
      withAWSToken t' $ \b -> do
        i <- liftIO $ testSize
        f a b i

testSize :: IO Int
testSize = do
  view <- lookupEnv "TEST_RELIABILITY_SIZE"
  let size = maybe 10 P.read view
  pure size

getMaxSuccess :: IO Int
getMaxSuccess = do
  view <- lookupEnv "TEST_RELIABILITY_SUCCESS"
  let size = maybe 5 P.read view
  pure size
