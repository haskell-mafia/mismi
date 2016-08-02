{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Mismi.EC2.Core.Data where

import           Mismi.EC2.Core.Data

import           P

import           Test.Mismi.EC2.Core.Arbitrary ()
import           Test.QuickCheck


prop_virtualization v =
  parseVirtualization (renderVirtualization v) === Just v

return []
tests = $quickCheckAll
