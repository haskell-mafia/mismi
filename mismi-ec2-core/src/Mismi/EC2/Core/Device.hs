{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Mismi.EC2.Core.Device (
    instanceDeviceMappings
  ) where

import           Mismi.EC2.Core.Data

-- http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/InstanceStorage.html#StorageOnInstanceTypes
instanceDeviceMappings :: MismiInstanceType -> [BlockDeviceMapping]
instanceDeviceMappings T1_Micro = devices0
instanceDeviceMappings T2_Nano = devices0
instanceDeviceMappings T2_Micro = devices0
instanceDeviceMappings T2_Small = devices0
instanceDeviceMappings T2_Medium = devices0
instanceDeviceMappings T2_Large = devices0

-- General Purpose - Current Generation
instanceDeviceMappings M3_Medium = devices1
instanceDeviceMappings M3_Large = devices1
instanceDeviceMappings M3_XLarge = devices2
instanceDeviceMappings M3_2XLarge = devices2

-- General Purpose - Previous Generation
instanceDeviceMappings M1_Small = devices1
instanceDeviceMappings M1_Medium = devices1
instanceDeviceMappings M1_Large = devices2
instanceDeviceMappings M1_XLarge = devices2

-- Compute Optimised - Current Generation
instanceDeviceMappings C3_Large = devices2
instanceDeviceMappings C3_XLarge = devices2
instanceDeviceMappings C3_2XLarge = devices2
instanceDeviceMappings C3_4XLarge = devices2
instanceDeviceMappings C3_8XLarge = devices2

-- Compute Optimised - Previous Generation
instanceDeviceMappings C1_Medium = devices1
instanceDeviceMappings C1_XLarge = devices4

-- Memory Optimised - Current Generation
instanceDeviceMappings R3_Large = devices1
instanceDeviceMappings R3_XLarge = devices1
instanceDeviceMappings R3_2XLarge = devices1
instanceDeviceMappings R3_4XLarge = devices1
instanceDeviceMappings R3_8XLarge = devices2

-- Memory Optimised - Previous Generation
instanceDeviceMappings M2_XLarge = devices2
instanceDeviceMappings M2_2XLarge = devices2
instanceDeviceMappings M2_4XLarge = devices2

-- High Storage Density
instanceDeviceMappings HS1_8XLarge = devices4

-- Storage Optimised
instanceDeviceMappings I2_XLarge = devices1
instanceDeviceMappings I2_2XLarge = devices2
instanceDeviceMappings I2_4XLarge = devices4
instanceDeviceMappings I2_8XLarge = devices4

-- Extra
instanceDeviceMappings C4_Large = devices0
instanceDeviceMappings C4_XLarge = devices0
instanceDeviceMappings C4_2XLarge = devices0
instanceDeviceMappings C4_4XLarge = devices0
instanceDeviceMappings C4_8XLarge = devices0

instanceDeviceMappings CC1_4XLarge = devices0
instanceDeviceMappings CC2_8XLarge = devices0

instanceDeviceMappings CG1_4XLarge = devices2

instanceDeviceMappings CR1_8XLarge = devices2

instanceDeviceMappings D2_2XLarge = devices4
instanceDeviceMappings D2_4XLarge = devices4
instanceDeviceMappings D2_8XLarge = devices4
instanceDeviceMappings D2_XLarge = devices4

instanceDeviceMappings G2_2XLarge = devices1
instanceDeviceMappings G2_8XLarge = devices2

instanceDeviceMappings HI1_4XLarge = devices2

instanceDeviceMappings M4_10XLarge = devices0
instanceDeviceMappings M4_2XLarge = devices0
instanceDeviceMappings M4_4XLarge = devices0
instanceDeviceMappings M4_XLarge = devices0
instanceDeviceMappings M4_Large = devices0

devices0 :: [BlockDeviceMapping]
devices0 = []

devices1 :: [BlockDeviceMapping]
devices1 = [
    BlockDeviceMapping "/dev/xvdb" "ephemeral0"
  ]

devices2 :: [BlockDeviceMapping]
devices2 = [
    BlockDeviceMapping "/dev/xvdb" "ephemeral0"
  , BlockDeviceMapping "/dev/xvdc" "ephemeral1"
  ]

devices4 :: [BlockDeviceMapping]
devices4 = [
    BlockDeviceMapping "/dev/xvdb" "ephemeral0"
  , BlockDeviceMapping "/dev/xvdc" "ephemeral1"
  , BlockDeviceMapping "/dev/xvdd" "ephemeral2"
  , BlockDeviceMapping "/dev/xvde" "ephemeral3"
  ]
