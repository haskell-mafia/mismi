{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mismi.EC2.Core.InstTypeToZone (
    sydAzForInstanceType
  ) where

import           Mismi.EC2.Core.MismiTypes
import           Mismi.EC2.Core.Ec2Types

-- | determine for az's for server types where the spot price is higher in ap-southeast-2
-- very much a hack until spot price is looked up dynamically using describeSpotPriceHistory
-- once implemented properly in mismi-ec2 this will be removed
sydAzForInstanceType :: MismiInstanceType -> [AvailabilityZone]
sydAzForInstanceType t =
  case t of
    M1_Large ->
      zonesAB
    M3_Large ->
      zonesAB
    M3_2XLarge ->
      zonesAB
    I2_2XLarge ->
      zonesAB
    I3_Large ->
      zonesBC
    I3_XLarge ->
      zonesBC
    I3_2XLarge ->
      zonesBC
    I3_4XLarge ->
      zonesBC
    I3_8XLarge ->
      zonesBC
    R3_4XLarge ->
      zonesAB
    R4_16XLarge ->
      zonesBC
    _ ->
      allZones
  where
    zonesAB  = [
               AvailabilityZone "ap-southeast-2a"
             , AvailabilityZone "ap-southeast-2b"
             ]
    zonesBC  = [
               AvailabilityZone "ap-southeast-2b"
             , AvailabilityZone "ap-southeast-2c"
             ]
    allZones = [
               AvailabilityZone "ap-southeast-2a"
             , AvailabilityZone "ap-southeast-2b"
             , AvailabilityZone "ap-southeast-2c"
             ]

