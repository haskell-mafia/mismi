{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Mismi.Arbitrary where

import           Network.AWS.Types

import           Test.QuickCheck


instance Arbitrary Region where
  -- Shorter list than possible, aws doesn't support all potential Regions.
  arbitrary = elements [Ireland, Tokyo, Singapore, Sydney, NorthCalifornia, Oregon]
