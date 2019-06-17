{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Mismi.EC2.Core.Device (
    instanceDeviceMappings
  , instanceStorage
  ) where

import           Mismi.EC2.Core.Data
import           Mismi.EC2.Core.MismiTypes

-- http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/InstanceStorage.html#instance-store-volumes
instanceDeviceMappings :: MismiInstanceType -> [BlockDeviceMapping]
instanceDeviceMappings t =
  case instanceStorage t of
    NoStorage -> devices0
    (InstanceStore _ _ _ NVMeSSD) -> devices0 -- nvme doesn't require mapping
    (InstanceStore d _ _ _) -> 
      case d of
        1 -> devices1
        2 -> devices2
        3 -> devices3
        4 -> devices4
        6 -> devices6
        8 -> devices8
        12 -> devices12
        24 -> devices24
        _ -> devices0

instanceStorage :: MismiInstanceType -> InstanceStorage
instanceStorage C1_Medium = InstanceStore 1 350 350 HDD
instanceStorage C1_XLarge = InstanceStore 4 420 1680 HDD
instanceStorage C3_2XLarge = InstanceStore 2 80 160 SSD
instanceStorage C3_4XLarge = InstanceStore 2 160 320 SSD
instanceStorage C3_8XLarge = InstanceStore 2 320 640 SSD
instanceStorage C3_Large = InstanceStore 2 16 32 SSD
instanceStorage C3_XLarge = InstanceStore 2 40 80 SSD
instanceStorage C4_2XLarge = NoStorage
instanceStorage C4_4XLarge = NoStorage
instanceStorage C4_8XLarge = NoStorage
instanceStorage C4_Large = NoStorage
instanceStorage C4_XLarge = NoStorage
instanceStorage C5_18XLarge = NoStorage
instanceStorage C5_2XLarge = NoStorage
instanceStorage C5_4XLarge = NoStorage
instanceStorage C5_9XLarge = NoStorage
instanceStorage C5_Large = NoStorage
instanceStorage C5_XLarge = NoStorage
instanceStorage C5d_18XLarge = InstanceStore 2 900 1800 NVMeSSD
instanceStorage C5d_2XLarge = InstanceStore 1 200 200 NVMeSSD
instanceStorage C5d_4XLarge = InstanceStore 1 400 400 NVMeSSD
instanceStorage C5d_9XLarge = InstanceStore 1 900 900 NVMeSSD
instanceStorage C5d_Large = InstanceStore 1 50 50 NVMeSSD
instanceStorage C5d_XLarge = InstanceStore 1 100 100 NVMeSSD
instanceStorage CC1_4XLarge = NoStorage
instanceStorage CC2_8XLarge = NoStorage
instanceStorage CG1_4XLarge = NoStorage
instanceStorage CR1_8XLarge = NoStorage
instanceStorage D2_2XLarge = InstanceStore 6 2000 12000 HDD
instanceStorage D2_4XLarge = InstanceStore 12 2000 24000 HDD
instanceStorage D2_8XLarge = InstanceStore 24 2000 48000 HDD
instanceStorage D2_XLarge = InstanceStore 3 2000 6000 HDD
instanceStorage F1_16XLarge = NoStorage
instanceStorage F1_2XLarge = NoStorage
instanceStorage G2_2XLarge = InstanceStore 1 60 60 SSD
instanceStorage G2_8XLarge = InstanceStore 2 120 240 SSD
instanceStorage G3_16XLarge = NoStorage
instanceStorage G3_4XLarge = NoStorage
instanceStorage G3_8XLarge = NoStorage
instanceStorage H1_16XLarge = NoStorage
instanceStorage H1_2XLarge = NoStorage
instanceStorage H1_4XLarge = NoStorage
instanceStorage H1_8XLarge = NoStorage
instanceStorage HI1_4XLarge = NoStorage
instanceStorage HS1_8XLarge = InstanceStore 24 2000 48000 HDD
instanceStorage I2_2XLarge = InstanceStore 2 800 1600 SSD
instanceStorage I2_4XLarge = InstanceStore 4 800 3200 SSD
instanceStorage I2_8XLarge = InstanceStore 8 800 6400 SSD
instanceStorage I2_XLarge = InstanceStore 1 800 800 SSD
instanceStorage I3_16XLarge = InstanceStore 8 1900 15200 NVMeSSD
instanceStorage I3_2XLarge = InstanceStore 1 1900 1900 NVMeSSD
instanceStorage I3_4XLarge = InstanceStore 2 1900 3800 NVMeSSD
instanceStorage I3_8XLarge = InstanceStore 4 1900 7600 NVMeSSD
instanceStorage I3_Large = InstanceStore 1 475 475 NVMeSSD
instanceStorage I3_Metal = NoStorage
instanceStorage I3_XLarge = InstanceStore 1 950 950 NVMeSSD
instanceStorage M1_Large = InstanceStore 2 420 840 HDD
instanceStorage M1_Medium = InstanceStore 1 410 410 HDD
instanceStorage M1_Small = InstanceStore 1 160 160 HDD
instanceStorage M1_XLarge = InstanceStore 4 420 1680 HDD
instanceStorage M2_2XLarge = InstanceStore 1 850 850 HDD
instanceStorage M2_4XLarge = InstanceStore 2 840 1680 HDD
instanceStorage M2_XLarge = InstanceStore 1 420 420 HDD
instanceStorage M3_2XLarge = InstanceStore 2 80 160 SSD
instanceStorage M3_Large = InstanceStore 1 32 32 SSD
instanceStorage M3_Medium = InstanceStore 1 4 4 SSD
instanceStorage M3_XLarge = InstanceStore 2 40 80 SSD
instanceStorage M4_10XLarge = NoStorage
instanceStorage M4_16XLarge = NoStorage
instanceStorage M4_2XLarge = NoStorage
instanceStorage M4_4XLarge = NoStorage
instanceStorage M4_Large = NoStorage
instanceStorage M4_XLarge = NoStorage
instanceStorage M5_12XLarge = NoStorage
instanceStorage M5_24XLarge = NoStorage
instanceStorage M5_2XLarge = NoStorage
instanceStorage M5_4XLarge = NoStorage
instanceStorage M5_Large = NoStorage
instanceStorage M5_XLarge = NoStorage
instanceStorage M5d_12XLarge = InstanceStore 2 900 1800 NVMeSSD
instanceStorage M5d_24XLarge = InstanceStore 4 900 3600 NVMeSSD
instanceStorage M5d_2XLarge = InstanceStore 1 300 300 NVMeSSD
instanceStorage M5d_4XLarge = InstanceStore 2 300 600 NVMeSSD
instanceStorage M5d_Large = InstanceStore 1 75 75 NVMeSSD
instanceStorage M5d_XLarge = InstanceStore 1 150 150 NVMeSSD
instanceStorage P2_16XLarge = NoStorage
instanceStorage P2_8XLarge = NoStorage
instanceStorage P2_XLarge = NoStorage
instanceStorage P3_16XLarge = NoStorage
instanceStorage P3_2XLarge = NoStorage
instanceStorage P3_8XLarge = NoStorage
instanceStorage R3_2XLarge = InstanceStore 1 160 160 SSD
instanceStorage R3_4XLarge = InstanceStore 1 320 320 SSD
instanceStorage R3_8XLarge = InstanceStore 2 320 640 SSD
instanceStorage R3_Large = InstanceStore 1 32 32 SSD
instanceStorage R3_XLarge = InstanceStore 1 80 80 SSD
instanceStorage R4_16XLarge = NoStorage
instanceStorage R4_2XLarge = NoStorage
instanceStorage R4_4XLarge = NoStorage
instanceStorage R4_8XLarge = NoStorage
instanceStorage R4_Large = NoStorage
instanceStorage R4_XLarge = NoStorage
instanceStorage R5_12XLarge = NoStorage
instanceStorage R5_16XLarge = NoStorage
instanceStorage R5_24XLarge = NoStorage
instanceStorage R5_2XLarge = NoStorage
instanceStorage R5_4XLarge = NoStorage
instanceStorage R5_8XLarge = NoStorage
instanceStorage R5_Large = NoStorage
instanceStorage R5_Metal = NoStorage
instanceStorage R5_XLarge = NoStorage
instanceStorage R5d_12XLarge = InstanceStore 2 900 1800 NVMeSSD
instanceStorage R5d_16XLarge = NoStorage
instanceStorage R5d_24XLarge = InstanceStore 4 900 3600 NVMeSSD
instanceStorage R5d_2XLarge = InstanceStore 1 300 300 NVMeSSD
instanceStorage R5d_4XLarge = InstanceStore 2 300 600 NVMeSSD
instanceStorage R5d_8XLarge = NoStorage
instanceStorage R5d_Large = InstanceStore 1 75 75 NVMeSSD
instanceStorage R5d_Metal = NoStorage
instanceStorage R5d_XLarge = InstanceStore 1 150 150 NVMeSSD
instanceStorage T1_Micro = NoStorage
instanceStorage T2_2XLarge = NoStorage
instanceStorage T2_Large = NoStorage
instanceStorage T2_Medium = NoStorage
instanceStorage T2_Micro = NoStorage
instanceStorage T2_Nano = NoStorage
instanceStorage T2_Small = NoStorage
instanceStorage T2_XLarge = NoStorage
instanceStorage X1_16XLarge = InstanceStore 1 1920 1920 SSD
instanceStorage X1_32XLarge = InstanceStore 2 1920 3840 SSD
instanceStorage X1e_16XLarge = InstanceStore 1 1920 1920 SSD
instanceStorage X1e_2XLarge = InstanceStore 1 240 240 SSD
instanceStorage X1e_32XLarge = InstanceStore 2 1920 3840 SSD
instanceStorage X1e_4XLarge = InstanceStore 1 480 480 SSD
instanceStorage X1e_8XLarge = InstanceStore 1 960 960 SSD
instanceStorage X1e_XLarge = InstanceStore 1 120 120 SSD
instanceStorage Z1d_12XLarge = InstanceStore 2 900 1800 NVMeSSD
instanceStorage Z1d_2XLarge = InstanceStore 1 300 300 NVMeSSD
instanceStorage Z1d_3XLarge = InstanceStore 1 450 450 NVMeSSD
instanceStorage Z1d_6XLarge = InstanceStore 1 900 900 NVMeSSD
instanceStorage Z1d_Large = InstanceStore 1 75 75 NVMeSSD
instanceStorage Z1d_XLarge = InstanceStore 1 150 150 NVMeSSD

-- fallback
-- instanceStorage _ = NoStorage


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

devices3 :: [BlockDeviceMapping]
devices3 = [
    BlockDeviceMapping "/dev/xvdb" "ephemeral0"
  , BlockDeviceMapping "/dev/xvdc" "ephemeral1"
  , BlockDeviceMapping "/dev/xvdd" "ephemeral2"
  ]

devices4 :: [BlockDeviceMapping]
devices4 = [
    BlockDeviceMapping "/dev/xvdb" "ephemeral0"
  , BlockDeviceMapping "/dev/xvdc" "ephemeral1"
  , BlockDeviceMapping "/dev/xvdd" "ephemeral2"
  , BlockDeviceMapping "/dev/xvde" "ephemeral3"
  ]

devices6 :: [BlockDeviceMapping]
devices6 = [
    BlockDeviceMapping "/dev/xvdb" "ephemeral0"
  , BlockDeviceMapping "/dev/xvdc" "ephemeral1"
  , BlockDeviceMapping "/dev/xvdd" "ephemeral2"
  , BlockDeviceMapping "/dev/xvde" "ephemeral3"
  , BlockDeviceMapping "/dev/xvdf" "ephemeral4"
  , BlockDeviceMapping "/dev/xvdg" "ephemeral5"
  ]

devices8 :: [BlockDeviceMapping]
devices8 = [
    BlockDeviceMapping "/dev/xvdb" "ephemeral0"
  , BlockDeviceMapping "/dev/xvdc" "ephemeral1"
  , BlockDeviceMapping "/dev/xvdd" "ephemeral2"
  , BlockDeviceMapping "/dev/xvde" "ephemeral3"
  , BlockDeviceMapping "/dev/xvdf" "ephemeral4"
  , BlockDeviceMapping "/dev/xvdg" "ephemeral5"
  , BlockDeviceMapping "/dev/xvdh" "ephemeral6"
  , BlockDeviceMapping "/dev/xvdi" "ephemeral7"
  ]

devices12 :: [BlockDeviceMapping]
devices12 = [
    BlockDeviceMapping "/dev/xvdb" "ephemeral0"
  , BlockDeviceMapping "/dev/xvdc" "ephemeral1"
  , BlockDeviceMapping "/dev/xvdd" "ephemeral2"
  , BlockDeviceMapping "/dev/xvde" "ephemeral3"
  , BlockDeviceMapping "/dev/xvdf" "ephemeral4"
  , BlockDeviceMapping "/dev/xvdg" "ephemeral5"
  , BlockDeviceMapping "/dev/xvdh" "ephemeral6"
  , BlockDeviceMapping "/dev/xvdi" "ephemeral7"
  , BlockDeviceMapping "/dev/xvdj" "ephemeral8"
  , BlockDeviceMapping "/dev/xvdk" "ephemeral9"
  , BlockDeviceMapping "/dev/xvdl" "ephemeral10"
  , BlockDeviceMapping "/dev/xvdm" "ephemeral11"
  ]

devices24 :: [BlockDeviceMapping]
devices24 = [
    BlockDeviceMapping "/dev/xvdb" "ephemeral0"
  , BlockDeviceMapping "/dev/xvdc" "ephemeral1"
  , BlockDeviceMapping "/dev/xvdd" "ephemeral2"
  , BlockDeviceMapping "/dev/xvde" "ephemeral3"
  , BlockDeviceMapping "/dev/xvdf" "ephemeral4"
  , BlockDeviceMapping "/dev/xvdg" "ephemeral5"
  , BlockDeviceMapping "/dev/xvdh" "ephemeral6"
  , BlockDeviceMapping "/dev/xvdi" "ephemeral7"
  , BlockDeviceMapping "/dev/xvdj" "ephemeral8"
  , BlockDeviceMapping "/dev/xvdk" "ephemeral9"
  , BlockDeviceMapping "/dev/xvdl" "ephemeral10"
  , BlockDeviceMapping "/dev/xvdm" "ephemeral11"
  , BlockDeviceMapping "/dev/xvdn" "ephemeral12"
  , BlockDeviceMapping "/dev/xvdo" "ephemeral13"
  , BlockDeviceMapping "/dev/xvdp" "ephemeral14"
  , BlockDeviceMapping "/dev/xvdq" "ephemeral15"
  , BlockDeviceMapping "/dev/xvdr" "ephemeral16"
  , BlockDeviceMapping "/dev/xvds" "ephemeral17"
  , BlockDeviceMapping "/dev/xvdt" "ephemeral18"
  , BlockDeviceMapping "/dev/xvdu" "ephemeral19"
  , BlockDeviceMapping "/dev/xvdv" "ephemeral20"
  , BlockDeviceMapping "/dev/xvdw" "ephemeral21"
  , BlockDeviceMapping "/dev/xvdx" "ephemeral22"
  , BlockDeviceMapping "/dev/xvdy" "ephemeral23"
  ]
