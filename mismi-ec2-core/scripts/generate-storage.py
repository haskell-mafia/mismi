#!/usr/bin/env python3
import os
import csv
from itertools import dropwhile, takewhile
import re

# https://pricing.us-east-1.amazonaws.com/offers/v1.0/aws/index.json
# /offers/v1.0/aws/AmazonEC2/current/index.json
# /offers/v1.0/aws/AmazonEC2/current/region_index.json
# /offers/v1.0/aws/AmazonEC2/20190612230233/ap-southeast-2/index.json

cmds = [
    "curl -o ec2-types.csv https://pricing.us-east-1.amazonaws.com/offers/v1.0/aws/AmazonEC2/current/ap-southeast-2/index.csv"
    , "head -6 ec2-types.csv | tail -1 > ap-southeast-2-ec2.csv"
    , """cat ec2-types.csv \\
       | grep -v '\"Reserved\"\\|\"Windows\"\\|\"Data Transfer\"\\|\"Storage\"\\|\"Dedicated\"\\|\"APS2-Reservation:' \\
       | grep '\"Compute Instance\"' \\
       | grep '\"RunInstances\"' \\
       | grep -v '\"UnusedCapacityReservation\"' \\
       | grep '\"Shared\"' \\
       | grep '\"Used\"' \\
       | grep '\"Linux\"' >> ap-southeast-2-ec2.csv"""
]
#

instance_types = {}

for cmd in cmds:
    os.system(cmd)

with open('ap-southeast-2-ec2.csv', newline='') as csvfile:
    reader = csv.DictReader(csvfile)
    for row in reader:
        instance_type = row['Instance Type']
        instance_types[instance_type] = row['Storage']


def write_start(outfile):
    outfile.write("""{-# LANGUAGE OverloadedStrings #-}
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
""")


def write_end(outfile):
    outfile.write("""


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
""")


def is_x(l):
    return not l.startswith('data MismiInstanceType =')


storage_re = r"^([0-9]+) x ([0-9,]+) ?(.+)?$"

with open('../src/Mismi/EC2/Core/Device.hs', mode='w') as outFile:
    write_start(outFile)

    with open('../src/Mismi/EC2/Core/Ec2Types.hs') as ec2TypeHs:
        lines = dropwhile(is_x, ec2TypeHs)
        line = next(lines)
        for curline in takewhile(lambda x: (x.startswith('  | ') or x.startswith('    ')) and x.find('deriving') == -1,
                                 lines):
            hs_type = curline[4:].strip()
            ec2_type = hs_type.lower().replace('_', '.')

            outFile.write(f"instanceStorage {hs_type} = ")
            if not ec2_type in instance_types:
                outFile.write(f"NoStorage\n")
                print(f"{ec2_type} unavailable - not in master list")
            else:
                storage = instance_types[ec2_type]
                matches = re.match(storage_re, storage)
                if matches is None:
                    line = "NoStorage"
                else:
                    disks = int(matches.group(1))
                    disk_size = int(matches.group(2).replace(',', ''))
                    disk_type = matches.group(3)
                    if disk_type is None:
                        disk_type = "HDD"
                    else:
                        disk_type = disk_type.replace(' ', '')
                    total_storage = disks * disk_size
                    line = f"InstanceStore {disks} {disk_size} {total_storage} {disk_type}"
                outFile.write(f"{line}\n")
                # print(f"{hs_type} -> {line}")

    write_end(outFile)
