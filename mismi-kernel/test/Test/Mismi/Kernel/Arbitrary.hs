{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Mismi.Kernel.Arbitrary where

import           Mismi.Kernel.Data

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


instance Arbitrary MismiRegion where
  arbitrary =
    arbitraryBoundedEnum
