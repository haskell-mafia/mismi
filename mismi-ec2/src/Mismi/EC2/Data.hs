{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Mismi.EC2.Data (
    module Mismi.EC2.Core.Data
  , fromMismiInstanceType
  , toMismiInstanceType
  , fromMismiVirtualizationType
  , toMismiVirtualizationType
  , fromMismiBlockDeviceMapping
  , toMismiTag
  , fromMismiTag
  ) where

import           Control.Lens ((.~), view)

import qualified Mismi.EC2.Amazonka as A
import           Mismi.EC2.Core.Data

import           P

fromMismiBlockDeviceMapping :: BlockDeviceMapping -> A.BlockDeviceMapping
fromMismiBlockDeviceMapping (BlockDeviceMapping n v) =
  A.blockDeviceMapping n
    & A.bdmVirtualName .~ Just v

fromMismiInstanceType :: MismiInstanceType -> A.InstanceType
fromMismiInstanceType m =
  case m of
    C1_Medium ->
      A.C1_Medium
    C1_XLarge ->
      A.C1_XLarge
    C3_2XLarge ->
      A.C3_2XLarge
    C3_4XLarge ->
      A.C3_4XLarge
    C3_8XLarge ->
      A.C3_8XLarge
    C3_Large ->
      A.C3_Large
    C3_XLarge ->
      A.C3_XLarge
    C4_2XLarge ->
      A.C4_2XLarge
    C4_4XLarge ->
      A.C4_4XLarge
    C4_8XLarge ->
      A.C4_8XLarge
    C4_Large ->
      A.C4_Large
    C4_XLarge ->
      A.C4_XLarge
    CC1_4XLarge ->
      A.CC1_4XLarge
    CC2_8XLarge ->
      A.CC2_8XLarge
    CG1_4XLarge ->
      A.CG1_4XLarge
    CR1_8XLarge ->
      A.CR1_8XLarge
    D2_2XLarge ->
      A.D2_2XLarge
    D2_4XLarge ->
      A.D2_4XLarge
    D2_8XLarge ->
      A.D2_8XLarge
    D2_XLarge ->
      A.D2_XLarge
    G2_2XLarge ->
      A.G2_2XLarge
    G2_8XLarge ->
      A.G2_8XLarge
    HI1_4XLarge ->
      A.HI1_4XLarge
    HS1_8XLarge ->
      A.HS1_8XLarge
    I2_2XLarge ->
      A.I2_2XLarge
    I2_4XLarge ->
      A.I2_4XLarge
    I2_8XLarge ->
      A.I2_8XLarge
    I2_XLarge ->
      A.I2_XLarge
    M1_Large ->
      A.M1_Large
    M1_Medium ->
      A.M1_Medium
    M1_Small ->
      A.M1_Small
    M1_XLarge ->
      A.M1_XLarge
    M2_2XLarge ->
      A.M2_2XLarge
    M2_4XLarge ->
      A.M2_4XLarge
    M2_XLarge ->
      A.M2_XLarge
    M3_2XLarge ->
      A.M3_2XLarge
    M3_Large ->
      A.M3_Large
    M3_Medium ->
      A.M3_Medium
    M3_XLarge ->
      A.M3_XLarge
    M4_10XLarge ->
      A.M4_10XLarge
    M4_2XLarge ->
      A.M4_2XLarge
    M4_4XLarge ->
      A.M4_4XLarge
    M4_Large ->
      A.M4_Large
    M4_XLarge ->
      A.M4_XLarge
    R3_2XLarge ->
      A.R3_2XLarge
    R3_4XLarge ->
      A.R3_4XLarge
    R3_8XLarge ->
      A.R3_8XLarge
    R3_Large ->
      A.R3_Large
    R3_XLarge ->
      A.R3_XLarge
    T1_Micro ->
      A.T1_Micro
    T2_Large ->
      A.T2_Large
    T2_Medium ->
      A.T2_Medium
    T2_Micro ->
      A.T2_Micro
    T2_Nano ->
      A.T2_Nano
    T2_Small ->
      A.T2_Small
    F1_16XLarge ->
      A.F1_16XLarge
    F1_2XLarge ->
      A.F1_2XLarge
    M4_16XLarge ->
      A.M4_16XLarge
    P2_16XLarge ->
      A.P2_16XLarge
    P2_8XLarge ->
      A.P2_8XLarge
    P2_XLarge ->
      A.P2_XLarge
    X1_16XLarge ->
      A.X1_16XLarge
    X1_32XLarge ->
      A.X1_32XLarge
    C5_18XLarge ->
      A.C5_18XLarge
    C5_2XLarge ->
      A.C5_2XLarge
    C5_4XLarge ->
      A.C5_4XLarge
    C5_9XLarge ->
      A.C5_9XLarge
    C5_Large ->
      A.C5_Large
    C5_XLarge ->
      A.C5_XLarge
    G3_16XLarge ->
      A.G3_16XLarge
    G3_4XLarge ->
      A.G3_4XLarge
    G3_8XLarge ->
      A.G3_8XLarge
    I3_16XLarge ->
      A.I3_16XLarge
    I3_2XLarge ->
      A.I3_2XLarge
    I3_4XLarge ->
      A.I3_4XLarge
    I3_8XLarge ->
      A.I3_8XLarge
    I3_Large ->
      A.I3_Large
    I3_XLarge ->
      A.I3_XLarge
    P3_16XLarge ->
      A.P3_16XLarge
    P3_2XLarge ->
      A.P3_2XLarge
    P3_8XLarge ->
      A.P3_8XLarge
    R4_16XLarge ->
      A.R4_16XLarge
    R4_2XLarge ->
      A.R4_2XLarge
    R4_4XLarge ->
      A.R4_4XLarge
    R4_8XLarge ->
      A.R4_8XLarge
    R4_Large ->
      A.R4_Large
    R4_XLarge ->
      A.R4_XLarge
    T2_2XLarge ->
      A.T2_2XLarge
    T2_XLarge ->
      A.T2_XLarge
    X1e_32XLarge ->
      A.X1e_32XLarge




toMismiInstanceType :: A.InstanceType -> MismiInstanceType
toMismiInstanceType i =
  case i of
    A.C1_Medium ->
      C1_Medium
    A.C1_XLarge ->
      C1_XLarge
    A.C3_2XLarge ->
      C3_2XLarge
    A.C3_4XLarge ->
      C3_4XLarge
    A.C3_8XLarge ->
      C3_8XLarge
    A.C3_Large ->
      C3_Large
    A.C3_XLarge ->
      C3_XLarge
    A.C4_2XLarge ->
      C4_2XLarge
    A.C4_4XLarge ->
      C4_4XLarge
    A.C4_8XLarge ->
      C4_8XLarge
    A.C4_Large ->
      C4_Large
    A.C4_XLarge ->
      C4_XLarge
    A.CC1_4XLarge ->
      CC1_4XLarge
    A.CC2_8XLarge ->
      CC2_8XLarge
    A.CG1_4XLarge ->
      CG1_4XLarge
    A.CR1_8XLarge ->
      CR1_8XLarge
    A.D2_2XLarge ->
      D2_2XLarge
    A.D2_4XLarge ->
      D2_4XLarge
    A.D2_8XLarge ->
      D2_8XLarge
    A.D2_XLarge ->
      D2_XLarge
    A.G2_2XLarge ->
      G2_2XLarge
    A.G2_8XLarge ->
      G2_8XLarge
    A.HI1_4XLarge ->
      HI1_4XLarge
    A.HS1_8XLarge ->
      HS1_8XLarge
    A.I2_2XLarge ->
      I2_2XLarge
    A.I2_4XLarge ->
      I2_4XLarge
    A.I2_8XLarge ->
      I2_8XLarge
    A.I2_XLarge ->
      I2_XLarge
    A.M1_Large ->
      M1_Large
    A.M1_Medium ->
      M1_Medium
    A.M1_Small ->
      M1_Small
    A.M1_XLarge ->
      M1_XLarge
    A.M2_2XLarge ->
      M2_2XLarge
    A.M2_4XLarge ->
      M2_4XLarge
    A.M2_XLarge ->
      M2_XLarge
    A.M3_2XLarge ->
      M3_2XLarge
    A.M3_Large ->
      M3_Large
    A.M3_Medium ->
      M3_Medium
    A.M3_XLarge ->
      M3_XLarge
    A.M4_10XLarge ->
      M4_10XLarge
    A.M4_2XLarge ->
      M4_2XLarge
    A.M4_4XLarge ->
      M4_4XLarge
    A.M4_Large ->
      M4_Large
    A.M4_XLarge ->
      M4_XLarge
    A.R3_2XLarge ->
      R3_2XLarge
    A.R3_4XLarge ->
      R3_4XLarge
    A.R3_8XLarge ->
      R3_8XLarge
    A.R3_Large ->
      R3_Large
    A.R3_XLarge ->
      R3_XLarge
    A.T1_Micro ->
      T1_Micro
    A.T2_Large ->
      T2_Large
    A.T2_Medium ->
      T2_Medium
    A.T2_Micro ->
      T2_Micro
    A.T2_Nano ->
      T2_Nano
    A.T2_Small ->
      T2_Small
    A.F1_16XLarge ->
      F1_16XLarge
    A.F1_2XLarge ->
      F1_2XLarge
    A.M4_16XLarge ->
      M4_16XLarge
    A.P2_16XLarge ->
      P2_16XLarge
    A.P2_8XLarge ->
      P2_8XLarge
    A.P2_XLarge ->
      P2_XLarge
    A.X1_16XLarge ->
      X1_16XLarge
    A.X1_32XLarge ->
      X1_32XLarge
    A.C5_18XLarge ->
      C5_18XLarge
    A.C5_2XLarge ->
      C5_2XLarge
    A.C5_4XLarge ->
      C5_4XLarge
    A.C5_9XLarge ->
      C5_9XLarge
    A.C5_Large ->
      C5_Large
    A.C5_XLarge ->
      C5_XLarge
    A.G3_16XLarge ->
      G3_16XLarge
    A.G3_4XLarge ->
      G3_4XLarge
    A.G3_8XLarge ->
      G3_8XLarge
    A.I3_16XLarge ->
      I3_16XLarge
    A.I3_2XLarge ->
      I3_2XLarge
    A.I3_4XLarge ->
      I3_4XLarge
    A.I3_8XLarge ->
      I3_8XLarge
    A.I3_Large ->
      I3_Large
    A.I3_XLarge ->
      I3_XLarge
    A.P3_16XLarge ->
      P3_16XLarge
    A.P3_2XLarge ->
      P3_2XLarge
    A.P3_8XLarge ->
      P3_8XLarge
    A.R4_16XLarge ->
      R4_16XLarge
    A.R4_2XLarge ->
      R4_2XLarge
    A.R4_4XLarge ->
      R4_4XLarge
    A.R4_8XLarge ->
      R4_8XLarge
    A.R4_Large ->
      R4_Large
    A.R4_XLarge ->
      R4_XLarge
    A.T2_2XLarge ->
      T2_2XLarge
    A.T2_XLarge ->
      T2_XLarge
    A.X1e_32XLarge ->
      X1e_32XLarge



fromMismiVirtualizationType :: MismiVirtualizationType -> A.VirtualizationType
fromMismiVirtualizationType v =
  case v of
    HVM ->
      A.HVM
    Paravirtual ->
      A.Paravirtual


toMismiVirtualizationType :: A.VirtualizationType -> MismiVirtualizationType
toMismiVirtualizationType v =
  case v of
    A.HVM ->
      HVM
    A.Paravirtual ->
      Paravirtual

toMismiTag :: A.Tag -> EC2Tag
toMismiTag e =
  EC2Tag
    (view A.tagKey e)
    (view A.tagValue e)

fromMismiTag :: EC2Tag -> A.Tag
fromMismiTag e =
  A.tag (tagKey e) (tagValue e)
