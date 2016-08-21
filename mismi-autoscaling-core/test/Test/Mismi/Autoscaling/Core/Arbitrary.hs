{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Mismi.Autoscaling.Core.Arbitrary where

import qualified Data.Text as T

import           Disorder.Core.Gen (genNonEmpty)
import           Disorder.Corpus

import           Mismi.Autoscaling.Core.Data

import           P

import           Test.Mismi.EC2.Core.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

instance Arbitrary ConfigurationName where
  arbitrary =
    ConfigurationName
      <$> genUniqueName

instance Arbitrary GroupName where
  arbitrary =
    GroupName
      <$> genUniqueName

genUniqueName :: Gen Text
genUniqueName = do
  m <- elements muppets
  c <- elements cooking
  n <- choose (9999, 99999 :: Int)
  pure $ T.intercalate "." [
      c
    , T.pack $ show n
    , m
    ]

instance Arbitrary ScalingInstance where
  arbitrary =
    ScalingInstance
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary ProtectedFromScaleIn where
  arbitrary =
    arbitraryBoundedEnum

instance Arbitrary Propagate where
  arbitrary =
    arbitraryBoundedEnum

instance Arbitrary MinInstances where
  arbitrary =
    MinInstances <$> choose (0, 10)

instance Arbitrary DesiredInstances where
  arbitrary =
    DesiredInstances <$> choose (0, 10)

instance Arbitrary MaxInstances where
  arbitrary =
    MaxInstances <$> choose (0, 10)

instance Arbitrary GroupTag where
  arbitrary =
    GroupTag
      <$> arbitrary
      <*> arbitrary

instance Arbitrary Group where
  arbitrary =
    Group
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> genNonEmpty arbitrary
      <*> arbitrary

instance Arbitrary GroupResult where
  arbitrary =
    GroupResult
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> genNonEmpty arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary AutoscalingMarket where
  arbitrary =
    oneof [
        pure OnDemand
      , (Spot . T.pack . show . (/ 10) . (fromIntegral :: Int -> Double)) <$> elements [1 .. 100]
      ]

instance Arbitrary Capacity where
  arbitrary =
    Capacity
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
