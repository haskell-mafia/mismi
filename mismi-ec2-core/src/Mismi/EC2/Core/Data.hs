{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mismi.EC2.Core.Data (
    InstanceId (..)
  , UserData (..)
  , SecurityGroupName (..)
  , SecurityGroupId (..)
  , LoadBalancer (..)
  , AvailabilityZone (..)
  , ImageId (..)
  , EC2Market (..)
  , MismiSpotInstanceType (..)
  , EC2Tag (..)
  , MismiInstanceType (..)
  , MismiVirtualizationType (..)
  , renderVirtualization
  , renderVirtualizationAws
  , parseVirtualization
  , virutalizationFor
  ) where

import           P


newtype InstanceId =
  InstanceId {
      instanceId :: Text
    } deriving (Eq, Show, Ord)

newtype UserData =
  UserData {
      userData :: Text
    } deriving (Eq, Show)

newtype SecurityGroupName =
  SecurityGroupName {
      securityGroupName :: Text
    } deriving (Eq, Show, Ord)

newtype SecurityGroupId =
  SecurityGroupId {
      securityGroupId :: Text
    } deriving (Eq, Show)

newtype LoadBalancer =
  LoadBalancer {
      loadBalancer :: Text
    } deriving (Eq, Show, Ord)

newtype AvailabilityZone =
  AvailabilityZone {
      availabilityZone :: Text
    } deriving (Eq, Show, Ord)

newtype ImageId =
  ImageId {
      imageId :: Text
    } deriving (Eq, Show, Ord)


data EC2Market =
    OnDemand
  | Spot !Text !MismiSpotInstanceType
  deriving (Eq, Show)

data EC2Tag =
  EC2Tag {
      tagKey :: !Text
    , tagVale :: !Text
    } deriving (Eq, Show, Ord)

-- | Mismi's view of spot instance types.
data MismiSpotInstanceType =
    OneTime
  | Persistent
    deriving (Eq, Show, Enum, Bounded)

-- | Mismi's view of available EC2 instance types.
data MismiInstanceType =
    C1_Medium
  | C1_XLarge
  | C3_2XLarge
  | C3_4XLarge
  | C3_8XLarge
  | C3_Large
  | C3_XLarge
  | C4_2XLarge
  | C4_4XLarge
  | C4_8XLarge
  | C4_Large
  | C4_XLarge
  | CC1_4XLarge
  | CC2_8XLarge
  | CG1_4XLarge
  | CR1_8XLarge
  | D2_2XLarge
  | D2_4XLarge
  | D2_8XLarge
  | D2_XLarge
  | G2_2XLarge
  | G2_8XLarge
  | HI1_4XLarge
  | HS1_8XLarge
  | I2_2XLarge
  | I2_4XLarge
  | I2_8XLarge
  | I2_XLarge
  | M1_Large
  | M1_Medium
  | M1_Small
  | M1_XLarge
  | M2_2XLarge
  | M2_4XLarge
  | M2_XLarge
  | M3_2XLarge
  | M3_Large
  | M3_Medium
  | M3_XLarge
  | M4_10XLarge
  | M4_2XLarge
  | M4_4XLarge
  | M4_Large
  | M4_XLarge
  | R3_2XLarge
  | R3_4XLarge
  | R3_8XLarge
  | R3_Large
  | R3_XLarge
  | T1_Micro
  | T2_Large
  | T2_Medium
  | T2_Micro
  | T2_Nano
  | T2_Small
    deriving (Eq, Show, Enum, Bounded)


-- | Mismi's view of available Virtualization types.
data MismiVirtualizationType =
    HVM
  | Paravirtual
    deriving (Eq, Show, Enum, Bounded)

renderVirtualization :: MismiVirtualizationType -> Text
renderVirtualization v =
  case v of
    HVM ->
      "hvm"
    Paravirtual ->
      "pv"

parseVirtualization :: Text -> Maybe MismiVirtualizationType
parseVirtualization t =
  case t of
    "hvm" ->
      Just HVM
    "pv" ->
      Just Paravirtual
    "paravirtual" ->
      Just Paravirtual
    _ ->
      Nothing

renderVirtualizationAws :: MismiVirtualizationType -> Text
renderVirtualizationAws v =
  case v of
    HVM ->
      "hvm"
    Paravirtual ->
      "paravirtual"

virutalizationFor :: MismiInstanceType -> MismiVirtualizationType
virutalizationFor itype =
  case itype of
    C1_Medium ->
      Paravirtual
    C1_XLarge ->
      Paravirtual
    C3_2XLarge ->
      HVM
    C3_4XLarge ->
      HVM
    C3_8XLarge ->
      HVM
    C3_Large ->
      HVM
    C3_XLarge ->
      HVM
    C4_2XLarge ->
      HVM
    C4_4XLarge ->
      HVM
    C4_8XLarge ->
      HVM
    C4_Large ->
      HVM
    C4_XLarge ->
      HVM
    CC1_4XLarge ->
      Paravirtual
    CC2_8XLarge ->
      Paravirtual
    CG1_4XLarge ->
      Paravirtual
    CR1_8XLarge ->
      Paravirtual
    D2_2XLarge ->
      HVM
    D2_4XLarge ->
      HVM
    D2_8XLarge ->
      HVM
    D2_XLarge ->
      HVM
    G2_2XLarge ->
      HVM
    G2_8XLarge ->
      HVM
    HI1_4XLarge ->
      Paravirtual
    HS1_8XLarge ->
      Paravirtual
    I2_2XLarge ->
      HVM
    I2_4XLarge ->
      HVM
    I2_8XLarge ->
      HVM
    I2_XLarge ->
      HVM
    M1_Large ->
      Paravirtual
    M1_Medium ->
      Paravirtual
    M1_Small ->
      Paravirtual
    M1_XLarge ->
      Paravirtual
    M2_2XLarge ->
      Paravirtual
    M2_4XLarge ->
      Paravirtual
    M2_XLarge ->
      Paravirtual
    M3_2XLarge ->
      HVM
    M3_Large ->
      HVM
    M3_Medium ->
      HVM
    M3_XLarge ->
      HVM
    M4_Large ->
      HVM
    M4_XLarge ->
      HVM
    M4_2XLarge ->
      HVM
    M4_4XLarge ->
      HVM
    M4_10XLarge ->
      HVM
    R3_2XLarge ->
      HVM
    R3_4XLarge ->
      HVM
    R3_8XLarge ->
      HVM
    R3_Large ->
      HVM
    R3_XLarge ->
      HVM
    T1_Micro ->
      Paravirtual
    T2_Large ->
      HVM
    T2_Medium ->
      HVM
    T2_Micro ->
      HVM
    T2_Small ->
      HVM
    T2_Nano ->
      HVM
