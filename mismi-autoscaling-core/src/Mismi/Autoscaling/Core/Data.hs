{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mismi.Autoscaling.Core.Data (
    ConfigurationName (..)
  , Configuration (..)
  , GroupName (..)
  , Group (..)
  , NonEmpty (..)
  , GroupResult (..)
  , DesiredInstances (..)
  , GroupTag (..)
  , EC2Tag (..)
  , Propagate (..)
  , renderSpotPrice
  , propagateToBool
  , increaseInstances
  , decreaseInstances
  ) where

import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Time (UTCTime)

import           Mismi.EC2.Core.Data
import           Mismi.IAM.Core.Data

import           P

newtype ConfigurationName =
  ConfigurationName {
      renderConfigurationName :: Text
    } deriving (Eq, Show, Ord)

data Configuration =
  Configuration {
      configurationName :: ConfigurationName
    , configurationImageId :: ImageId
    , configurationInstanceType :: MismiInstanceType
    , configurationSecurityGroups :: [SecurityGroupName]
    , configurationIam :: IamRole
    , configurationUserData :: UserData
    , configurationMarket :: AutoscalingMarket
    } deriving (Eq, Show, Ord)

data AutoscalingMarket =
    OnDemand
  | Spot !Text
    deriving (Eq, Show, Ord)

renderSpotPrice :: AutoscalingMarket -> Maybe Text
renderSpotPrice a =
  case a of
    OnDemand ->
      Nothing
    Spot v ->
      Just v

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

propagateToBool :: Propagate -> Bool
propagateToBool p =
  case p of
    Propagate ->
      True
    DontPropagate ->
      False

decreaseInstances :: DesiredInstances -> DesiredInstances
decreaseInstances d =
  DesiredInstances $ (desiredInstances d) - 1

increaseInstances :: DesiredInstances -> DesiredInstances
increaseInstances d =
  DesiredInstances $ (desiredInstances d) + 1
