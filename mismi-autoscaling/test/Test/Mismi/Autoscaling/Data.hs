{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Mismi.Autoscaling.Data where

import           Mismi.Autoscaling.Data

import           P

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

prop_parse_protection_error = conjoin [
    parseProtectionError "The instance i-ef2f5d40 is not part of Auto Scaling group stewed.27197.animal."
      === InstanceProtectionNotFound
  , parseProtectionError "The instance i-ef2f5d40 is not in InService or EnteringStandby or Standby."
      === InstanceProtectionInvalidState
  ]

prop_parse_protection_error_unknown u =
  parseProtectionError u === InstanceProtectionUnknownError u

return []
tests = $quickCheckAll
