{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Mismi.EC2.Commands where

import           Data.Maybe

import           Mismi.EC2.Commands
import           Mismi.EC2.Data

import           P

import           System.IO (IO)

import           Test.Mismi
import           Test.QuickCheck


prop_findSecurityGroupByName_found :: Property
prop_findSecurityGroupByName_found = testAWS $ do
  mid <- findSecurityGroupByName (SecurityGroupName "ci.ci.db")
  return $ isJust mid

prop_findSecurityGroupByName_not_found :: Property
prop_findSecurityGroupByName_not_found = testAWS $ do
  mid <- findSecurityGroupByName (SecurityGroupName "not-found")
  return $ mid === Nothing


return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 2 })
