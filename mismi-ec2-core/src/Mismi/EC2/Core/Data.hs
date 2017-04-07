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
  , BlockDeviceMapping (..)
  , encodeUserData
  , decodeUserData
  , renderVirtualization
  , renderVirtualizationAws
  , parseVirtualization
  , virtualizationFor
  , renderMismiInstanceType
  , parseMismiInstanceType
  ) where

import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           P

newtype InstanceId =
  InstanceId {
      instanceId :: Text
    } deriving (Eq, Show, Ord)

newtype UserData =
  UserData {
      userData :: Text
    } deriving (Eq, Show, Ord)

encodeUserData :: UserData -> Text
encodeUserData =
  T.decodeUtf8 . Base64.encode . T.encodeUtf8 . userData

decodeUserData :: Text -> Either Text UserData
decodeUserData =
  bimap T.pack (UserData . T.decodeUtf8) . Base64.decode . T.encodeUtf8

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
    EC2OnDemand
  | EC2Spot !Text !MismiSpotInstanceType
    deriving (Eq, Show)

data EC2Tag =
  EC2Tag {
      tagKey :: !Text
    , tagValue :: !Text
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
  | M4_16XLarge
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
  | P2_XLarge
  | P2_8XLarge
  | P2_16XLarge
  | X1_16XLarge
  | X1_32XLarge
  | F1_2XLarge
  | F1_16XLarge
    deriving (Eq, Show, Ord, Enum, Bounded)


-- | Mismi's view of available Virtualization types.
data MismiVirtualizationType =
    HVM
  | Paravirtual
    deriving (Eq, Show, Enum, Bounded)

-- | Mismi's view of block devices
data BlockDeviceMapping =
  BlockDeviceMapping {
      blockDeviceMappingDeviceName :: Text
    , blockDeviceMappingVirtualName :: Text
    } deriving (Eq, Show)

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

virtualizationFor :: MismiInstanceType -> MismiVirtualizationType
virtualizationFor itype =
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
    M4_16XLarge ->
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
    P2_XLarge ->
      HVM
    P2_8XLarge ->
      HVM
    P2_16XLarge ->
      HVM
    X1_16XLarge ->
      HVM
    X1_32XLarge ->
      HVM
    F1_2XLarge ->
      HVM -- Guess
    F1_16XLarge ->
      HVM -- Guess


renderMismiInstanceType :: MismiInstanceType -> Text
renderMismiInstanceType m =
  case m of
    C1_Medium ->
      "c1.medium"
    C1_XLarge ->
      "c1.xlarge"
    C3_2XLarge ->
      "c3.2xlarge"
    C3_4XLarge ->
      "c3.4xlarge"
    C3_8XLarge ->
      "c3.8xlarge"
    C3_Large ->
      "c3.large"
    C3_XLarge ->
      "c3.xlarge"
    C4_2XLarge ->
      "c4.2xlarge"
    C4_4XLarge ->
      "c4.4xlarge"
    C4_8XLarge ->
      "c4.8xlarge"
    C4_Large ->
      "c4.large"
    C4_XLarge ->
      "c4.xlarge"
    CC1_4XLarge ->
      "cc1.4xlarge"
    CC2_8XLarge ->
      "cc2.8xlarge"
    CG1_4XLarge ->
      "cg1.4xlarge"
    CR1_8XLarge ->
      "cr1.8xlarge"
    D2_2XLarge ->
      "d2.2xlarge"
    D2_4XLarge ->
      "d2.4xlarge"
    D2_8XLarge ->
      "d2.8xlarge"
    D2_XLarge ->
      "d2.xlarge"
    G2_2XLarge ->
      "g2.2xlarge"
    G2_8XLarge ->
      "g2.8xlarge"
    HI1_4XLarge ->
      "hi1.4xlarge"
    HS1_8XLarge ->
      "hs1.8xlarge"
    I2_2XLarge ->
      "i2.2xlarge"
    I2_4XLarge ->
      "i2.4xlarge"
    I2_8XLarge ->
      "i2.8xlarge"
    I2_XLarge ->
      "i2.xlarge"
    M1_Large ->
      "m1.large"
    M1_Medium ->
      "m1.medium"
    M1_Small ->
      "m1.small"
    M1_XLarge ->
      "m1.xlarge"
    M2_2XLarge ->
      "m2.2xlarge"
    M2_4XLarge ->
      "m2.4xlarge"
    M2_XLarge ->
      "m2.xlarge"
    M3_2XLarge ->
      "m3.2xlarge"
    M3_Large ->
      "m3.large"
    M3_Medium ->
      "m3.medium"
    M3_XLarge ->
      "m3.xlarge"
    M4_Large ->
      "m4.large"
    M4_XLarge ->
      "m4.xlarge"
    M4_2XLarge ->
      "m4.2xlarge"
    M4_4XLarge ->
      "m4.4xlarge"
    M4_10XLarge ->
      "m4.10xlarge"
    M4_16XLarge ->
      "m4.16xlarge"
    R3_2XLarge ->
      "r3.2xlarge"
    R3_4XLarge ->
      "r3.4xlarge"
    R3_8XLarge ->
      "r3.8xlarge"
    R3_Large ->
      "r3.large"
    R3_XLarge ->
      "r3.xlarge"
    T1_Micro ->
      "t1.micro"
    T2_Large ->
      "t2.large"
    T2_Medium ->
      "t2.medium"
    T2_Micro ->
      "t2.micro"
    T2_Small ->
      "t2.small"
    T2_Nano ->
      "t2.nano"
    P2_XLarge ->
      "p2.xlarge"
    P2_8XLarge ->
      "p2.8xlarge"
    P2_16XLarge ->
      "p2.16xlarge"
    X1_16XLarge ->
      "x1.16xlarge"
    X1_32XLarge ->
      "x1.32xlarge"
    F1_2XLarge ->
      "f1.2xLarge"
    F1_16XLarge ->
      "f1.16xlarge"

parseMismiInstanceType :: Text -> Maybe MismiInstanceType
parseMismiInstanceType m =
  case m of
    "c1.medium" ->
      Just C1_Medium
    "c1.xlarge" ->
      Just C1_XLarge
    "c3.2xlarge" ->
      Just C3_2XLarge
    "c3.4xlarge" ->
      Just C3_4XLarge
    "c3.8xlarge" ->
      Just C3_8XLarge
    "c3.large" ->
      Just C3_Large
    "c3.xlarge" ->
      Just C3_XLarge
    "c4.2xlarge" ->
      Just C4_2XLarge
    "c4.4xlarge" ->
      Just C4_4XLarge
    "c4.8xlarge" ->
      Just C4_8XLarge
    "c4.large" ->
      Just C4_Large
    "c4.xlarge" ->
      Just C4_XLarge
    "cc1.4xlarge" ->
      Just CC1_4XLarge
    "cc2.8xlarge" ->
      Just CC2_8XLarge
    "cg1.4xlarge" ->
      Just CG1_4XLarge
    "cr1.8xlarge" ->
      Just CR1_8XLarge
    "d2.2xlarge" ->
      Just D2_2XLarge
    "d2.4xlarge" ->
      Just D2_4XLarge
    "d2.8xlarge" ->
      Just D2_8XLarge
    "d2.xlarge" ->
      Just D2_XLarge
    "g2.2xlarge" ->
      Just G2_2XLarge
    "g2.8xlarge" ->
      Just G2_8XLarge
    "hi1.4xlarge" ->
      Just HI1_4XLarge
    "hs1.8xlarge" ->
      Just HS1_8XLarge
    "i2.2xlarge" ->
      Just I2_2XLarge
    "i2.4xlarge" ->
      Just I2_4XLarge
    "i2.8xlarge" ->
      Just I2_8XLarge
    "i2.xlarge" ->
      Just I2_XLarge
    "m1.large" ->
      Just M1_Large
    "m1.medium" ->
      Just M1_Medium
    "m1.small" ->
      Just M1_Small
    "m1.xlarge" ->
      Just M1_XLarge
    "m2.2xlarge" ->
      Just M2_2XLarge
    "m2.4xlarge" ->
      Just M2_4XLarge
    "m2.xlarge" ->
      Just M2_XLarge
    "m3.2xlarge" ->
      Just M3_2XLarge
    "m3.large" ->
      Just M3_Large
    "m3.medium" ->
      Just M3_Medium
    "m3.xlarge" ->
      Just M3_XLarge
    "m4.large" ->
      Just M4_Large
    "m4.xlarge" ->
      Just M4_XLarge
    "m4.2xlarge" ->
      Just M4_2XLarge
    "m4.4xlarge" ->
      Just M4_4XLarge
    "m4.10xlarge" ->
      Just M4_10XLarge
    "m4.16xlarge" ->
      Just M4_16XLarge
    "r3.2xlarge" ->
      Just R3_2XLarge
    "r3.4xlarge" ->
      Just R3_4XLarge
    "r3.8xlarge" ->
      Just R3_8XLarge
    "r3.large" ->
      Just R3_Large
    "r3.xlarge" ->
      Just R3_XLarge
    "t1.micro" ->
      Just T1_Micro
    "t2.large" ->
      Just T2_Large
    "t2.medium" ->
      Just T2_Medium
    "t2.micro" ->
      Just T2_Micro
    "t2.small" ->
      Just T2_Small
    "t2.nano" ->
      Just T2_Nano
    "p2.xlarge" ->
      Just P2_XLarge
    "p2.8xlarge" ->
      Just P2_8XLarge
    "p2.16xlarge" ->
      Just P2_16XLarge
    "x1.16xlarge" ->
      Just X1_16XLarge
    "x1.32xlarge" ->
      Just X1_32XLarge
    "f1.2xLarge" ->
      Just F1_2XLarge
    "f1.16xlarge" ->
      Just F1_16XLarge
    _ ->
      Nothing
