{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Mismi.SQS.Core.Arbitrary where

import           Data.Text as T

import           Mismi.Kernel.Data
import           Mismi.SQS.Core.Data

import           P

import           Test.Mismi.Kernel.Arbitrary ()
import           Test.QuickCheck


instance Arbitrary Queue where
  arbitrary = Queue <$> arbitrary <*> sqsRegions

instance Arbitrary QueueName where
  arbitrary = fmap (QueueName . T.pack) . vectorOf 80 . elements
    $ ['a'..'z'] <> ['0'..'9'] <> "-_"

sqsRegions :: Gen MismiRegion
sqsRegions =
  elements [IrelandRegion, TokyoRegion, SingaporeRegion, SydneyRegion, NorthCaliforniaRegion, OregonRegion, NorthVirginiaRegion]
