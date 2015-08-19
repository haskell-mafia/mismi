{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Mismi (
    testAWS
  ) where

import           Control.Monad.Catch

import           Disorder.Core.IO

import           Mismi

import           P

import           Test.Mismi.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Property


testAWS :: Testable a => Region -> AWS a -> Property
testAWS r a = testIO $
  (runAWSWithRegion r a >> return succeeded) `catch` (return . exception "AWS action failed with exception")
