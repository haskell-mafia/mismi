{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Mismi (
    testAWS
  , enableTests
  , runAWSDefaultRegion
  ) where

import           Control.Monad.Catch (throwM)

import           Disorder.Core.IO (testIO)

import           Mismi

import           P
import           Prelude (String)

import           System.Environment (lookupEnv)
import           System.IO (IO)

import           Test.Mismi.Arbitrary ()
import           Test.QuickCheck (Property, Testable)

import           X.Control.Monad.Trans.Either (eitherT)


testAWS :: Testable a => AWS a -> Property
testAWS =
  testIO . runAWSDefaultRegion

-- Default to Sydney for tests only, production should fail without the environment variable
runAWSDefaultRegion :: AWS a -> IO a
runAWSDefaultRegion a = do
  r <- eitherT (const $ pure Sydney) pure getRegionFromEnv
  e <- discoverAWSEnvWithRegion r
  eitherT throwM pure $ runAWS e a

enableTests :: String -> [IO Bool]  -> [IO Bool] -> IO [IO Bool]
enableTests k false true = do
  d <- lookupEnv k
  pure $ bool false true $
    maybe
      True
      (\s ->
        case s of
          "true" ->
            True
          "1" ->
            True
          "false" ->
            False
          "0" ->
            False
          _ ->
            True)
      d
