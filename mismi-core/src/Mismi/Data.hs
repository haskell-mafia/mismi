{-# LANGUAGE NoImplicitPrelude #-}
module Mismi.Data (
    module Mismi.Kernel.Data
  , fromMismiRegion
  , toMismiRegion
  ) where

import           Control.Monad.Trans.AWS (Region (..))

import           Mismi.Kernel.Data


fromMismiRegion :: MismiRegion -> Region
fromMismiRegion r =
  case r of
    IrelandRegion ->
      Ireland
    FrankfurtRegion ->
      Frankfurt
    TokyoRegion ->
      Tokyo
    SingaporeRegion ->
      Singapore
    SydneyRegion ->
      Sydney
    BeijingRegion ->
      Beijing
    NorthVirginiaRegion ->
      NorthVirginia
    NorthCaliforniaRegion ->
      NorthCalifornia
    OregonRegion ->
      Oregon
    GovCloudRegion ->
      GovCloud
    GovCloudFIPSRegion ->
      GovCloudFIPS
    SaoPauloRegion ->
      SaoPaulo
    OhioRegion ->
      Ohio
    SeoulRegion ->
      Seoul
    MumbaiRegion ->
      Mumbai

toMismiRegion :: Region -> MismiRegion
toMismiRegion r =
  case r of
    Ireland ->
      IrelandRegion
    Frankfurt ->
      FrankfurtRegion
    Tokyo ->
      TokyoRegion
    Singapore ->
      SingaporeRegion
    Sydney ->
      SydneyRegion
    Beijing ->
      BeijingRegion
    NorthVirginia ->
      NorthVirginiaRegion
    NorthCalifornia ->
      NorthCaliforniaRegion
    Oregon ->
      OregonRegion
    GovCloud ->
      GovCloudRegion
    GovCloudFIPS ->
      GovCloudFIPSRegion
    SaoPaulo ->
      SaoPauloRegion
    Ohio ->
      OhioRegion
    Seoul ->
      SeoulRegion
    Mumbai ->
      MumbaiRegion
