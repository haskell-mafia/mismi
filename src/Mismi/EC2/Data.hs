{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Mismi.EC2.Data (
    InstanceId (..)
  , UserData (..)
  ) where

import           Data.Text

import           P


newtype InstanceId =
  InstanceId {
      unInstanceId :: Text
  } deriving (Eq, Show)

newtype UserData =
  UserData {
      unUserData :: Text
  } deriving (Eq, Show)
