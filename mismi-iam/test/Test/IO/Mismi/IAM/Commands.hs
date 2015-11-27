{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Mismi.IAM.Commands where

import           Disorder.Core.Property ((=/=))

import           Mismi.IAM (lookupCurrentAccountNumber)

import           P

import           System.IO (IO)

import           Test.Mismi
import           Test.QuickCheck


prop_lookupCurrentAccountNumber :: Property
prop_lookupCurrentAccountNumber = testAWS $ do
  man <- lookupCurrentAccountNumber
  pure $ man =/= Nothing

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1 })
