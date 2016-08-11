{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Mismi.Autoscaling.Control where

import           Control.Monad.IO.Class (liftIO)

import           Disorder.Core.IO

import qualified Data.Text.IO as T

import           Mismi
import           Mismi.Autoscaling.Commands
import           Mismi.Autoscaling.Core.Data
import           Mismi.EC2.Core.Data
import           Mismi.IAM.Core.Data

import           P

import           Test.Mismi.Autoscaling.Core.Arbitrary ()
import           Test.Mismi
import           Test.QuickCheck

testConf :: Testable a => (ConfigurationName -> AWS a) -> Property
testConf action =
  property $ \c ->
    testIO . runAWSDefaultRegion $
      withConf c action

withConf :: ConfigurationName -> (ConfigurationName -> AWS a) -> AWS a
withConf conf action =
  let
    delete = do
      liftIO . T.putStrLn $ "Deleting configuration: " <> renderConfigurationName conf
      deleteConfiguration conf
  in
    awsBracket_ (pure ()) delete (action conf)

withGroup :: ConfigurationName -> GroupName -> (ConfigurationName -> GroupName -> AWS a) -> AWS a
withGroup conf group action =
  let
    delete = do
      liftIO . T.putStrLn $ "Deleting group: " <> renderGroupName group
      deleteGroup group
      liftIO . T.putStrLn $ "Deleting configuration: " <> renderConfigurationName conf
      deleteConfiguration conf
  in
    awsBracket_ (pure ()) delete (action conf group)

testGroup :: Testable a => (ConfigurationName -> GroupName -> AWS a) -> Property
testGroup f =
  property $ \c g -> testIO . runAWSDefaultRegion $
    withGroup c g f

group' :: ConfigurationName -> GroupName -> Int -> Group
group' cn gn i =
  Group
   gn
   cn
   (DesiredInstances i)
   [GroupTag (EC2Tag "environment" "test") Propagate]
   defaultAvailabilityZones
   []

-- TODO env vars for vars
conf' :: ConfigurationName -> Configuration
conf' cn =
  Configuration
    cn
    (ImageId "ami-bc3611df")
    T2_Nano
    [SecurityGroupName "ci.ci.node"]
    (IamRole "ci.ci.node")
    (UserData "something not empty")
    OnDemand

defaultAvailabilityZones :: NonEmpty AvailabilityZone
defaultAvailabilityZones =
  AvailabilityZone "ap-southeast-2a" :| [
      AvailabilityZone "ap-southeast-2b"
    , AvailabilityZone "ap-southeast-2c"
    ]
