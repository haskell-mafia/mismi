{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Mismi.Autoscaling.Core.Data where

import           Mismi.Autoscaling.Core.Data

import           P

import           Test.Mismi.Autoscaling.Core.Arbitrary ()
import           Test.QuickCheck

prop_scale_in p =
  protectedFromScaleInFromBool (protectedFromScaleInToBool p) === p

prop_propagate p =
  propagateFromBool (propagateToBool p) === p

return []
tests = $quickCheckAll
