{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Mismi.S3.Control where

import           Control.Monad.Trans.AWS

import           Mismi.S3.Aws.Control

import           P

import           System.IO

import           Test.Mismi.S3 ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


prop_endpoint :: Region -> Property
prop_endpoint r =
  (epToRegion $ regionToEp r) === Just r

return []
tests :: IO Bool
tests = $quickCheckAll
