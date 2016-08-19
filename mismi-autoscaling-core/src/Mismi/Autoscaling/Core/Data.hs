{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mismi.Autoscaling.Core.Data (
    ConfigurationName (..)
  , Configuration (..)
  , AutoscalingMarket (..)
  , GroupName (..)
  , Group (..)
  , NonEmpty (..)
  , GroupResult (..)
  , MinInstances (..)
  , DesiredInstances (..)
  , MaxInstances (..)
  , Capacity (..)
  , GroupTag (..)
  , EC2Tag (..)
  , Propagate (..)
  , renderMarket
  , renderSpotPrice
  , propagateToBool
  , propagateFromBool
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

renderMarket :: AutoscalingMarket -> Text
renderMarket m =
  case m of
    OnDemand ->
      "ondemand"
    Spot t ->
      t

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
    , groupCapacity :: Capacity
    , groupGroupTags :: [GroupTag]
    , groupAvailabilityZones :: NonEmpty AvailabilityZone
    , groupLoadBalancers :: [LoadBalancer]
    } deriving (Eq, Show, Ord)

data GroupResult =
  GroupResult {
      groupResultName :: !GroupName
    , groupResultConfName :: !ConfigurationName
    , groupResultCapacity :: !Capacity
    , groupResultAvailabilityZones :: !(NonEmpty AvailabilityZone)
    , groupResultLoadBalances :: ![LoadBalancer]
    , groupResultInstances :: ![InstanceId]
    , groupResultCreationTime :: !UTCTime
    , groupResultTags :: ![GroupTag]
    } deriving (Eq, Show)

newtype MinInstances =
  MinInstances {
      minInstances :: Int
    } deriving (Eq, Show, Ord)

newtype DesiredInstances =
  DesiredInstances {
      desiredInstances :: Int
    } deriving (Eq, Show, Ord)

newtype MaxInstances =
  MaxInstances {
      maxInstances :: Int
    } deriving (Eq, Show, Ord)

data Capacity =
  Capacity {
      minCapacity :: !MinInstances
    , desiredCapacity :: !DesiredInstances
    , maxCapacity :: !MaxInstances
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

propagateFromBool :: Bool -> Propagate
propagateFromBool p =
  case p of
    True ->
      Propagate
    False ->
      DontPropagate

decreaseInstances :: DesiredInstances -> DesiredInstances
decreaseInstances d =
  DesiredInstances $ (desiredInstances d) - 1

increaseInstances :: DesiredInstances -> DesiredInstances
increaseInstances d =
  DesiredInstances $ (desiredInstances d) + 1
