{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
module Mismi.Autoscaling.Commands (
    createConfiguration
  , createGroup
  , describeGroups
  , describeGroup
  , describeConfigurations
  , describeConfigurationsRaw
  , describeConfiguration
  , describeConfigurationRaw
  , deleteGroup
  , deleteConfiguration
  , updateDesiredInstances
  , updateMinMaxInstances
  , describeLoadBalancers
  , detachLoadBalancers
  , attachLoadBalancer
  , updateTags
  , lockInstances
  , unlockInstances
  , InstanceProtectionError (..)
  ) where

import           Control.Lens ((^.), (.~))

import           Control.Retry (limitRetries, constantDelay)

import           Data.Conduit
import qualified Data.Conduit.List as DC

import           Mismi.Amazonka (AWS, paginate, send, toText)
import qualified Mismi.Autoscaling.Amazonka as A
import           Mismi.Autoscaling.Core.Data
import           Mismi.Autoscaling.Data
import           Mismi.Autoscaling.Error
import           Mismi.EC2.Data (fromMismiInstanceType)
import           Mismi.EC2.Core.Data (LoadBalancer (..), AvailabilityZone (..), InstanceId (..))
import           Mismi.EC2.Core.Data (ImageId (..), SecurityGroupName (..), encodeUserData)
import           Mismi.EC2.Core.Device (instanceDeviceMappings)
import           Mismi.IAM.Core.Data (IamRole (..))

import           P

import           X.Control.Monad.Trans.Either (EitherT, newEitherT)

createConfiguration :: Configuration -> AWS ()
createConfiguration c = do
  continueIfExists . void . send $ A.createLaunchConfiguration (renderConfigurationName $ configurationName c)
    & A.clcImageId .~ Just (imageId $ configurationImageId c)
    & A.clcInstanceType .~ Just (toText . fromMismiInstanceType $ configurationInstanceType c)
    & A.clcSpotPrice .~ (renderSpotPrice $ configurationMarket c)
    & A.clcSecurityGroups .~ (securityGroupName <$> configurationSecurityGroups c)
    & A.clcIAMInstanceProfile .~ Just (iamRole $ configurationIam c)
    & A.clcUserData .~ Just (encodeUserData $ configurationUserData c)
    & A.clcBlockDeviceMappings .~ fmap fromMismiBlockDeviceMapping (instanceDeviceMappings $ configurationInstanceType c)

createGroup :: Group -> AWS ()
createGroup g =
  continueIfExists . void . send $ A.createAutoScalingGroup
    (renderGroupName $ groupName g)
    (minInstances . minCapacity $ groupCapacity g)
    (maxInstances . maxCapacity $ groupCapacity g)
      & A.casgAvailabilityZones .~ Just (availabilityZone <$> groupAvailabilityZones g)
      & A.casgTags .~ (toTags (groupName g) (groupGroupTags g))
      & A.casgLaunchConfigurationName .~ Just (renderConfigurationName $ groupConfigurationName g)
      & A.casgDesiredCapacity .~ Just (desiredInstances . minimumDesiredInstances . desiredCapacity $ groupCapacity g)
      & A.casgLoadBalancerNames .~ (loadBalancer <$> groupLoadBalancers g)
      & A.casgNewInstancesProtectedFromScaleIn .~ Just True

describeGroups :: EitherT GroupResultError AWS [GroupResult]
describeGroups =
  newEitherT $ mapM toGroupResult <$> describeGroupsRaw

describeGroupsRaw :: AWS [A.AutoScalingGroup]
describeGroupsRaw = do
  l <- paginate A.describeAutoScalingGroups $$ DC.foldMap (^. A.dasgrsAutoScalingGroups)
  pure $ filter (\z -> not . isJust $ z ^. A.asgStatus) l -- Remove ASGs in a terminating state.

describeGroup :: GroupName -> EitherT GroupResultError AWS (Maybe GroupResult)
describeGroup g =
  newEitherT $ mapM toGroupResult <$> describeGroupRaw g

describeGroupRaw :: GroupName -> AWS (Maybe A.AutoScalingGroup)
describeGroupRaw g =
  catchValidationError Nothing $ do
    l <- paginate (A.describeAutoScalingGroups & A.dasgAutoScalingGroupNames .~ [renderGroupName g]) $$
      DC.foldMap (^. A.dasgrsAutoScalingGroups)
    let r = filter (\z -> not . isJust $ z ^. A.asgStatus) l -- Remove ASGs in a terminating state.
    pure $ case r of
      x : [] ->
        Just x
      _ ->
        Nothing

describeConfigurations :: EitherT ConfigurationError AWS [Configuration]
describeConfigurations =
  newEitherT $ mapM toConfiguration <$> describeConfigurationsRaw

describeConfigurationsRaw :: AWS [A.LaunchConfiguration]
describeConfigurationsRaw = do
  l <- paginate A.describeLaunchConfigurations $$ DC.foldMap (^. A.dlcrsLaunchConfigurations)
  pure l

describeConfiguration :: ConfigurationName -> EitherT ConfigurationError AWS (Maybe Configuration)
describeConfiguration c =
  newEitherT $ mapM toConfiguration <$> describeConfigurationRaw c

describeConfigurationRaw :: ConfigurationName -> AWS (Maybe A.LaunchConfiguration)
describeConfigurationRaw c = do
  l <- paginate (A.describeLaunchConfigurations & A.dlcLaunchConfigurationNames .~ [renderConfigurationName c]) $$
    DC.foldMap (^. A.dlcrsLaunchConfigurations)
  pure $ case l of
    x : [] ->
      Just x
    _ ->
      Nothing

deleteGroup :: GroupName -> AWS ()
deleteGroup gn =
  catchValidationError () $ void . retry
    (constantDelay 10000000 <> limitRetries 10) {- 10 seconds -}
    [scalingInProgress, resourceInUse]
    . send $ ((A.deleteAutoScalingGroup $ renderGroupName gn) & A.dasgForceDelete .~ Just True)

deleteConfiguration :: ConfigurationName -> AWS ()
deleteConfiguration cn =
  catchValidationError () $ void . retry
    (constantDelay 5000000 <> limitRetries 10) {- 5 seconds -}
    [resourceInUse]
    . send $ (A.deleteLaunchConfiguration $ renderConfigurationName cn)

updateDesiredInstances :: GroupName -> DesiredInstances -> AWS ()
updateDesiredInstances gn di =
  when (desiredInstances di >= 0) . void . send $
    A.setDesiredCapacity (renderGroupName gn) (desiredInstances di)

updateMinMaxInstances :: GroupName -> MinInstances -> MaxInstances -> AWS ()
updateMinMaxInstances g a b =
  void . send $ A.updateAutoScalingGroup (renderGroupName g)
    & A.uasgMinSize .~ Just (minInstances a)
    & A.uasgMaxSize .~ Just (maxInstances b)

describeLoadBalancers :: GroupName -> AWS [LoadBalancer]
describeLoadBalancers n = do
  l <- send $ A.describeLoadBalancers (renderGroupName n)
  let
    r = (\z -> (z ^. A.lbsLoadBalancerName) >>= \q ->
      case (z ^. A.lbsState == Just "Removing") of
        False ->
          Nothing
        True ->
          Just q) <$> (l ^. A.dlbrsLoadBalancers)
  pure . fmap LoadBalancer $ catMaybes r

detachLoadBalancers :: GroupName -> AWS ()
detachLoadBalancers n = do
  l <- describeLoadBalancers n
  when (not . null $ l) $
    void . send $ A.detachLoadBalancers (renderGroupName n)
      & A.dAutoScalingGroupName .~ (renderGroupName n)
      & A.dLoadBalancerNames .~ fmap loadBalancer l

attachLoadBalancer :: GroupName -> LoadBalancer -> AWS ()
attachLoadBalancer n l = do
  void . send $ A.attachLoadBalancers (renderGroupName n)
    & A.albAutoScalingGroupName .~ (renderGroupName n)
    & A.albLoadBalancerNames .~ [loadBalancer l]

updateTags :: GroupName -> [GroupTag] -> AWS ()
updateTags g t =
  void . send $ A.createOrUpdateTags & A.coutTags .~ (toTags g t)

lockInstances :: GroupName -> [InstanceId] -> EitherT InstanceProtectionError AWS ()
lockInstances n is =
  setInstanceProtection n is ProtectedFromScaleIn

unlockInstances :: GroupName -> [InstanceId] -> EitherT InstanceProtectionError AWS ()
unlockInstances n is =
  setInstanceProtection n is NotProtectedFromScaleIn


-- Can only set protection on instances that have the lifecycle state
-- of 'InService', 'EnteringStandby' or 'Standby'.
setInstanceProtection :: GroupName -> [InstanceId] -> ProtectedFromScaleIn -> EitherT InstanceProtectionError AWS ()
setInstanceProtection n is p =
  when (not $ null is) .
    newEitherT . catchValidationErrorMessage (Left . parseProtectionError) . fmap Right .
      void . send $ A.setInstanceProtection (renderGroupName n) (protectedFromScaleInToBool p)
        & A.sipInstanceIds .~ (fmap instanceId is)
