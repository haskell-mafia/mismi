{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Mismi.S3.Control where

import           Control.Monad.IO.Class

import           Mismi.S3.Commands

import           P

import           System.IO

import           Test.Mismi
import           Test.Mismi.S3
import           Test.QuickCheck

prop_finalizer = testAWS $ do
  r <- liftIO . runAWSDefaultRegion $ do
    a <- newAddress
    writeOrFail a ""
    pure $ a
  e <- exists r
  pure $ e === False

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
