{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.IO.Mismi.EC2.Arbitrary where

import           Mismi.EC2.Data

import           P

import           Test.QuickCheck

instance Arbitrary VirtualizationType where
  arbitrary =
    elements [HVM, Paravirtual]

instance Arbitrary SecurityGroupName where
  arbitrary =
    elements . fmap SecurityGroupName $ [
      "default"
    , "test.hub.db"
    ]
