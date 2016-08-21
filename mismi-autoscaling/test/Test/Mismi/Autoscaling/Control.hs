{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Mismi.Autoscaling.Control where

import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Disorder.Core.IO

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Mismi
import           Mismi.Autoscaling.Commands
import           Mismi.Autoscaling.Core.Data
import           Mismi.EC2.Core.Data
import           Mismi.IAM.Core.Data

import           P

import           System.Environment (lookupEnv)
import           System.IO (IO)

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
   (Capacity (MinInstances 0) (DesiredInstances i) (MaxInstances 10))
   [GroupTag (EC2Tag "environment" "test") Propagate]
   defaultAvailabilityZones
   []

conf' :: MonadIO m => ConfigurationName -> m Configuration
conf' cn =
  Configuration
    <$> pure cn
    <*> liftIO testImageId
    <*> pure T2_Nano
    <*> ((:[]) <$> liftIO testSecurityGroup)
    <*> liftIO testIamRole
    <*> pure (UserData "something not empty")
    <*> pure OnDemand

testIamRole :: IO IamRole
testIamRole =
  IamRole . T.pack . fromMaybe "ci.ci.node" <$> lookupEnv "AWS_TEST_IAM_ROLE"

testSecurityGroup :: IO SecurityGroupName
testSecurityGroup =
  SecurityGroupName . T.pack . fromMaybe "ci.ci.node" <$>
    lookupEnv "AWS_TEST_SECURITY_GROUP"

testImageId :: IO ImageId
testImageId =
  ImageId . T.pack . fromMaybe "ami-bc3611df" <$> lookupEnv "AWS_TEST_IMAGE_ID"

defaultAvailabilityZones :: NonEmpty AvailabilityZone
defaultAvailabilityZones =
  AvailabilityZone "ap-southeast-2a" :| [
      AvailabilityZone "ap-southeast-2b"
    , AvailabilityZone "ap-southeast-2c"
    ]
