{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Mismi.EC2.Data (
    module X
  , InstanceId (..)
  , UserData (..)
  ) where

import           Data.Text

import qualified Network.AWS.EC2 as X
import           Network.AWS.EC2 hiding (UserData)

import           P


newtype InstanceId =
  InstanceId {
      unInstanceId :: Text
  } deriving (Eq, Show, Ord)

newtype UserData =
  UserData {
      unUserData :: Text
  } deriving (Eq, Show)
