{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Mismi.EC2.Data (
    InstanceId (..)
  , UserData (..)
  , VirtualizationType (..)
  , SecurityGroupName(..)
  , SecurityGroupId(..)
  , renderVirtualization
  , renderVirtualizationAws
  , parseVirtualization
  ) where

import           P

import           Mismi.EC2.Amazonka (VirtualizationType (..))

newtype InstanceId =
  InstanceId {
      instanceId :: Text
  } deriving (Eq, Show, Ord)

newtype UserData =
  UserData {
      userData :: Text
  } deriving (Eq, Show)

renderVirtualization :: VirtualizationType -> Text
renderVirtualization v =
  case v of
    HVM ->
      "hvm"
    Paravirtual ->
      "pv"

parseVirtualization :: Text -> Maybe VirtualizationType
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

renderVirtualizationAws :: VirtualizationType -> Text
renderVirtualizationAws v =
  case v of
    HVM ->
      "hvm"
    Paravirtual ->
      "paravirtual"


newtype SecurityGroupName =
  SecurityGroupName {
      securityGroupName :: Text
    } deriving (Eq, Show)

newtype SecurityGroupId =
  SecurityGroupId {
      securityGroupId :: Text
    } deriving (Eq, Show)
