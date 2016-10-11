{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Mismi.EC2.Data where

import           Mismi.EC2.Data

import           P

import           Test.Mismi.EC2.Core.Arbitrary ()
import           Test.QuickCheck


prop_instance_type v =
  toMismiInstanceType (fromMismiInstanceType v) === v

prop_virtualization v =
  toMismiVirtualizationType (fromMismiVirtualizationType v) === v

prop_tag e =
  toMismiTag (fromMismiTag e) === e

return []
tests = $quickCheckAll
