{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Mismi.EC2.Arbitrary where

import           Mismi.EC2.Data

import           Test.QuickCheck

instance Arbitrary VirtualizationType where
  arbitrary =
    elements [HVM, Paravirtual]
