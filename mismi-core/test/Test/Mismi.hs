{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Mismi (
    testAWS
  , runAWSDefaultRegion
  ) where

import           Control.Monad.Catch

import           Disorder.Core.IO

import           Mismi

import           P

import           System.IO

import           Test.Mismi.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Property


testAWS :: Testable a => AWS a -> Property
testAWS a = testIO $
  (runAWSDefaultRegion a >> return succeeded) `catch` (return . exception "AWS action failed with exception")

-- Default to Sydney for tests only, production should fail without the environment variable
runAWSDefaultRegion :: AWS a -> IO a
runAWSDefaultRegion a = do
  mr <- getRegionFromEnv
  e <- newEnv (fromMaybe Sydney mr) Discover
  runAWS e a
