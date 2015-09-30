{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Mismi (
    testAWS
  , runAWSDefaultRegion
  ) where

import           Disorder.Core.IO

import           Mismi
import           Mismi.Amazonka (newEnv, Credentials (..))

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
  mr <- getRegionFromEnv
  e <- newEnv (fromMaybe Sydney mr) Discover
  runAWS e a
