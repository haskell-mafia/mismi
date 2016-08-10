{-# LANGUAGE NoImplicitPrelude  #-}
module Mismi.Autoscaling.Commands (
  ) where

import           Control.Lens ((^.), (.~), view)

import           Control.Retry (limitRetries, constantDelay)

import           Mismi.Amazonka
import qualified Mismi.Autoscaling.Amazonka as A
import           Mismi.Autoscaling.Core.Data
import           Mismi.Autoscaling.Data
import           Mismi.EC2.Core.Data (LoadBalancer (..), AvailabilityZone (..))

import           P

createConfiguration :: Configuration -> AWS ()
createConfiguration c = do
  continueIfExists . void . send $ A.createLaunchConfiguration (renderConfigurationName $ confName c)
    & A.clcImageId .~ Just (imageId $ confImageId c)
    & A.clcInstanceType .~ Just (toText $ confInstanceType c)
    & A.clcSpotPrice .~ (renderSpotPrice $ confMarket c)
    & A.clcSecurityGroups .~ (securityGroup <$> confSecurityGroups c)
    & A.clcIAMInstanceProfile .~ Just (iam $ confIam c)
    & A.clcUserData .~ Just (T.decodeUtf8 . Base64.encode . T.encodeUtf8 . ordinanceToText $ confUserData c)
    & A.clcBlockDeviceMappings .~ autoscaleMappings (instanceDeviceMappings $ confInstanceType c)

createGroup :: Group -> AWS ()
createGroup g =
  continueIfExists . void . send $ A.createAutoScalingGroup
    (renderGroupName $ groupName g)
    0
    10
      & A.casgAvailabilityZones .~ Just (availabilityZone <$> groupAvailabilityZones g)
      & A.casgTags .~ (groupTags (groupName g) (groupGroupTags g))
      & A.casgLaunchConfigurationName .~ Just (renderConfigurationName $ groupConfigurationName g)
      & A.casgDesiredCapacity .~ Just (desiredInstances . minimumDesiredInstances $ groupDesiredInstances g)
      & A.casgLoadBalancerNames .~ (loadBalancer <$> groupLoadBalancers g)

deleteGroup :: GroupName -> AWS ()
deleteGroup gn =
  void . retry
    (constantDelay 10000000 <> limitRetries 10) {- 10 seconds -}
    [scalingInProgress, resourceInUse]
    . send $ ((A.deleteAutoScalingGroup $ renderGroupName gn) & A.dasgForceDelete .~ Just True)

deleteConfiguration :: ConfigurationName -> AWS ()
deleteConfiguration cn =
  void . retry
    (constantDelay 5000000 <> limitRetries 10) {- 5 seconds -}
    [resourceInUse]
    . send $ (A.deleteLaunchConfiguration $ renderConfigurationName cn)


setCapacity :: GroupName -> DesiredInstances -> AWS ()
setCapacity gn di =
  when (desiredInstances di >= 0) . void . send $
    A.setDesiredCapacity (renderGroupName gn) (desiredInstances di)

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
    void . send $ A.detachLoadBalancers
      & A.dAutoScalingGroupName .~ Just (renderGroupName n)
      & A.dLoadBalancerNames .~ fmap loadBalancer l

attachLoadBalancer :: GroupName -> LoadBalancer -> AWS ()
attachLoadBalancer n l = do
  void . send $ A.attachLoadBalancers
    & A.albAutoScalingGroupName .~ Just (renderGroupName n)
    & A.albLoadBalancerNames .~ [loadBalancer l]

updateTags :: GroupName -> [GroupTag] -> AWS ()
updateTags g t =
  void . send $ A.createOrUpdateTags & A.coutTags .~ fmap (groupTag g) t
