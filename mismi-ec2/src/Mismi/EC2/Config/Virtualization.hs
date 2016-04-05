{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mismi.EC2.Config.Virtualization (
    A.InstanceType (..)
  , virtualizationFor
  ) where

import qualified Mismi.EC2.Amazonka as A

import           Mismi.EC2.Data

-- Provide a "preferred" virtualization for each instance type.
--  If it supports HVM prefer HVM, otherwise prefer PV.
virtualizationFor :: A.InstanceType -> VirtualizationType
virtualizationFor itype =
  case itype of
    A.C1_Medium ->
      Paravirtual
    A.C1_XLarge ->
      Paravirtual
    A.C3_2XLarge ->
      HVM
    A.C3_4XLarge ->
      HVM
    A.C3_8XLarge ->
      HVM
    A.C3_Large ->
      HVM
    A.C3_XLarge ->
      HVM
    A.C4_2XLarge ->
      HVM
    A.C4_4XLarge ->
      HVM
    A.C4_8XLarge ->
      HVM
    A.C4_Large ->
      HVM
    A.C4_XLarge ->
      HVM
    A.CC1_4XLarge ->
      Paravirtual
    A.CC2_8XLarge ->
      Paravirtual
    A.CG1_4XLarge ->
      Paravirtual
    A.CR1_8XLarge ->
      Paravirtual
    A.D2_2XLarge ->
      HVM
    A.D2_4XLarge ->
      HVM
    A.D2_8XLarge ->
      HVM
    A.D2_XLarge ->
      HVM
    A.G2_2XLarge ->
      HVM
#if MIN_VERSION_amazonka(1,4,0)
    A.G2_8XLarge ->
      HVM
#endif
    A.HI1_4XLarge ->
      Paravirtual
    A.HS1_8XLarge ->
      Paravirtual
    A.I2_2XLarge ->
      HVM
    A.I2_4XLarge ->
      HVM
    A.I2_8XLarge ->
      HVM
    A.I2_XLarge ->
      HVM
    A.M1_Large ->
      Paravirtual
    A.M1_Medium ->
      Paravirtual
    A.M1_Small ->
      Paravirtual
    A.M1_XLarge ->
      Paravirtual
    A.M2_2XLarge ->
      Paravirtual
    A.M2_4XLarge ->
      Paravirtual
    A.M2_XLarge ->
      Paravirtual
    A.M3_2XLarge ->
      HVM
    A.M3_Large ->
      HVM
    A.M3_Medium ->
      HVM
    A.M3_XLarge ->
      HVM
    A.M4_Large ->
      HVM
    A.M4_XLarge ->
      HVM
    A.M4_2XLarge ->
      HVM
    A.M4_4XLarge ->
      HVM
    A.M4_10XLarge ->
      HVM
    A.R3_2XLarge ->
      HVM
    A.R3_4XLarge ->
      HVM
    A.R3_8XLarge ->
      HVM
    A.R3_Large ->
      HVM
    A.R3_XLarge ->
      HVM
    A.T1_Micro ->
      Paravirtual
    A.T2_Large ->
      HVM
    A.T2_Medium ->
      HVM
    A.T2_Micro ->
      HVM
    A.T2_Small ->
      HVM
#if MIN_VERSION_amazonka(1,4,0)
    A.T2_Nano ->
      HVM
#endif
