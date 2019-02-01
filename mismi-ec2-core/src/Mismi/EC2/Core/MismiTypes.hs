{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mismi.EC2.Core.MismiTypes (
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
  , BlockDeviceMapping (..)
  , MismiVirtualizationType (..)
  , encodeUserData
  , decodeUserData
  , renderVirtualization
  , renderVirtualizationAws
  , parseVirtualization
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
