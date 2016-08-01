{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings #-}
module Mismi.EC2.Commands (
    findSecurityGroupByName
  ) where

import           Control.Lens (view, (.~))

import           Mismi.Amazonka
import           Mismi.EC2.Data

import           Mismi.EC2.Amazonka

import           P

findSecurityGroupByName :: SecurityGroupName -> AWS (Maybe SecurityGroupId)
findSecurityGroupByName (SecurityGroupName n) = do
  r <- send $ describeSecurityGroups & dsgsFilters .~ [filter' "group-name" & fValues .~ [n]]
  return . fmap (SecurityGroupId . view sgGroupId) . listToMaybe . view dsgrsSecurityGroups $ r
