{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mismi.Autoscaling.Core.Data (
    ConfigurationName (..)
  , GroupName (..)
  , Group (..)
  , NonEmpty (..)
  , GroupResult (..)
  , DesiredInstances (..)
  , GroupTag (..)
  , EC2Tag (..)
  , Propagate (..)
  , increaseInstances
  , decreaseInstances
  ) where

import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Time (UTCTime)

import           P

import           Mismi.EC2.Core.Data

newtype ConfigurationName =
  ConfigurationName {
      renderConfigurationName :: Text
    } deriving (Eq, Show, Ord)

newtype GroupName =
  GroupName {
      renderGroupName :: Text
    } deriving (Eq, Show, Ord)

data Group =
  Group {
      groupName :: GroupName
    , groupConfigurationName :: ConfigurationName
    , groupDesiredInstances :: DesiredInstances
    , groupGroupTags :: [GroupTag]
    , groupAvailabilityZones :: NonEmpty AvailabilityZone
    , groupLoadBalancers :: [LoadBalancer]
    } deriving (Eq, Show, Ord)

data GroupResult =
  GroupResult {
      groupResultName :: GroupName
    , groupResultConfName :: ConfigurationName
    , groupResultCapacity :: DesiredInstances
    , groupResultAvailabilityZones :: [AvailabilityZone]
    , groupResultLoadBalances :: [LoadBalancer]
    , groupResultInstances :: [InstanceId]
    , groupResultCreationTime :: UTCTime
    } deriving (Eq, Show)

newtype DesiredInstances =
  DesiredInstances {
      desiredInstances :: Int
    } deriving (Eq, Show, Ord)

data GroupTag =
  GroupTag {
      groupTag :: !EC2Tag
    , groupTagPropagateAtLaunch :: !Propagate
    } deriving (Eq, Show, Ord)

data Propagate =
    Propagate
  | DontPropagate
    deriving (Eq, Show, Ord, Enum, Bounded)


decreaseInstances :: DesiredInstances -> DesiredInstances
decreaseInstances d =
  DesiredInstances $ (desiredInstances d) - 1

increaseInstances :: DesiredInstances -> DesiredInstances
increaseInstances d =
  DesiredInstances $ (desiredInstances d) + 1
