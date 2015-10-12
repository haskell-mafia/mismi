{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Mismi (
    testAWS
  , runAWSDefaultRegion
  ) where

import           Control.Monad.Catch
import           Control.Monad.Trans.Either

import           Disorder.Core.IO

import           Mismi

import           P

import           System.IO

import           Test.Mismi.Arbitrary ()
import           Test.QuickCheck


testAWS :: Testable a => AWS a -> Property
testAWS =
  testIO . runAWSDefaultRegion

-- Default to Sydney for tests only, production should fail without the environment variable
runAWSDefaultRegion :: AWS a -> IO a
runAWSDefaultRegion a = do
  r <- eitherT (const $ pure Sydney) pure getRegionFromEnv
  e <- discoverAWSEnvWithRegion r
  eitherT throwM pure $ runAWS e a
