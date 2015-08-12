{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Mismi (
    testAWS
  ) where

import           Control.Monad.Trans.Either

import           Disorder.Core
import           Disorder.Core.IO

import           Mismi

import           P

import           System.IO

import           Test.QuickCheck
import           Test.Mismi.Arbitrary ()


testAWS :: Testable a => Region -> AWS a -> Property
testAWS r a =
  testIO $ do
    e <- runEitherT $ runAWS r a
    pure $ either (\e' -> failWith $ "Property failed [" <> awsErrorRender e' <> "].") property e
