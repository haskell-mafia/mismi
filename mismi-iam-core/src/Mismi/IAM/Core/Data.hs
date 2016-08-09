{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mismi.IAM.Core.Data (
    IamRole (..)
  ) where

import           P

newtype IamRole =
  IamRole {
      iamRole :: Text
    } deriving (Eq, Show, Ord)
