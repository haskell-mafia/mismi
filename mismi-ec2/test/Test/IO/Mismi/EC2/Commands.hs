{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Mismi.EC2.Commands where

import           Data.Maybe

import           Mismi.EC2.Commands

import           P

import           System.IO (IO)

import           Test.IO.Mismi.EC2.Arbitrary ()
import           Test.Mismi
import           Test.QuickCheck


prop_findSecurityGroupByName_found :: SecurityGroupName -> Property
prop_findSecurityGroupByName_found sg = testAWS $ do
  mid <- findSecurityGroupByName sg
  return $ isJust mid

prop_findSecurityGroupByName_not_found :: Property
prop_findSecurityGroupByName_not_found = testAWS $ do
  mid <- findSecurityGroupByName (SecurityGroupName "not-found")
  return $ mid === Nothing


return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 2 })
