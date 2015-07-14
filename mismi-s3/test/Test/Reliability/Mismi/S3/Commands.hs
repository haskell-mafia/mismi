{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Reliability.Mismi.S3.Commands where

import           Control.Monad.IO.Class

import           Disorder.Core.IO

import           Mismi.S3.Control
import           Mismi.S3.Data

import           P
import qualified Prelude as P

import           System.IO
import           System.Environment

import           Test.Mismi.S3
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

testS3 :: Testable a => (Address -> Int -> S3Action a) -> Property
testS3 f =
  property $ \t -> testIO .
    runS3WithDefaults . withToken t $ \a -> do
      i <- liftIO $ testSize
      f a i

testSize :: IO Int
testSize = do
  view <- lookupEnv "TEST_RELIABILITY_SIZE"
  pure $ maybe 10 P.read view

return []
tests :: IO Bool
tests = do
  s <- getMaxSuccess
  $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = s })

getMaxSuccess :: IO Int
getMaxSuccess = do
    view <- lookupEnv "TEST_RELIABILITY_SUCCESS"
    pure $ maybe 5 P.read view
