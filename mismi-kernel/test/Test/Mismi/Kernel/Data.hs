{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Mismi.Kernel.Data where

import           Disorder.Core

import           Mismi.Kernel.Data

import           P

import           Test.QuickCheck
import           Test.Mismi.Kernel.Arbitrary ()


prop_tripping_region =
  tripping renderMismiRegion parseMismiRegion

return []
tests = $quickCheckAll
