{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Mismi.Data where

import           Disorder.Core

import           Mismi.Data

import           P

import           Test.Mismi.Arbitrary ()

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

prop_tripping_region =
  tripping fromMismiRegion (Just . toMismiRegion)

return []
tests = $quickCheckAll
