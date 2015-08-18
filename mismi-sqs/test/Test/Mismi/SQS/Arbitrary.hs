{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Mismi.SQS.Arbitrary where

import           Data.Text as T

import           Mismi.SQS.Data

import           P

import           Test.Mismi.Arbitrary ()
import           Test.QuickCheck


instance Arbitrary Queue where
  arbitrary = Queue <$> arbitrary <*> arbitrary

instance Arbitrary QueueName where
  arbitrary = fmap (QueueName . T.pack) . vectorOf 80 . elements
    $ ['a'..'z'] <> ['0'..'9'] <> "-_"

