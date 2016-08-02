{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Mismi.EC2.Core.Arbitrary where

import qualified Data.Text as T

import           Disorder.Corpus

import           Mismi.EC2.Core.Data

import           P

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


instance Arbitrary InstanceId where
  arbitrary =
    InstanceId <$> elements muppets

instance Arbitrary EC2Tag where
  arbitrary =
    EC2Tag
      <$> elements cooking
      <*> elements boats

instance Arbitrary SecurityGroupName where
  arbitrary =
    SecurityGroupName <$> elements simpsons

instance Arbitrary ImageId where
  arbitrary =
    ImageId
      <$> (T.pack . ("ami-" <>) <$> vectorOf 8 (oneof [choose ('0', '9'), choose ('a', 'z')]))

instance Arbitrary EC2Market where
  arbitrary =
    oneof [
        pure OnDemand
      , Spot <$> elements ["0.05", "1.10", "0.40"] <*> elements [OneTime, Persistent]
      ]

instance Arbitrary AvailabilityZone where
  arbitrary =
    elements [
        AvailabilityZone "ap-southeast-2a"
      , AvailabilityZone "ap-southeast-2b"
      , AvailabilityZone "ap-southeast-2c"
      ]

instance Arbitrary MismiSpotInstanceType where
  arbitrary =
    arbitraryBoundedEnum

instance Arbitrary MismiInstanceType where
  arbitrary =
    arbitraryBoundedEnum

instance Arbitrary MismiVirtualizationType where
  arbitrary =
    arbitraryBoundedEnum
