{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Mismi.Autoscaling.Commands where

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T

import           Disorder.Corpus

import           Mismi (AWS)
import           Mismi.Autoscaling.Core.Data
import           Mismi.Autoscaling.Commands

import           P

import           Test.Mismi.Autoscaling.Control
import           Test.Mismi.Autoscaling.Core.Arbitrary ()
import           Test.QuickCheck

import           Twine.Data (seconds)
import           Twine.Snooze (snooze)

import           X.Control.Monad.Trans.Either

prop_conf_create = once . testConf $ \cn -> do
  conf <- conf' cn
  createConfiguration conf
  r <- runEitherT $ describeConfiguration cn
  pure $ r === Right (Just conf)

prop_conf_delete = once . testConf $ \c -> do
  conf <- conf' c
  deleteConfiguration c
  createConfiguration conf
  deleteConfiguration c >> deleteConfiguration c
  r <- runEitherT $ describeConfiguration c
  pure $ r === Right Nothing

prop_conf_list = once . testConf $ \c ->do
  conf <- conf' c
  createConfiguration conf
  l <- runEitherT describeConfigurations
  pure $ fmap (elem conf) l === Right True

prop_group_create = once . testGroup $ \c g -> do
  conf <- conf' c
  createConfiguration conf
  createGroup $ group' c g 0
  r <- runEitherT $ describeGroup g
  pure $ (fmap . fmap) groupResultName r === Right (Just g)

prop_group_list = once . testGroup $ \c g -> do
  conf <- conf' c
  createConfiguration conf
  createGroup $ group' c g 0
  r <- runEitherT $ describeGroups
  let names = (fmap . fmap) groupResultName r
  pure $ fmap (elem g) names === Right True

prop_group_delete = once . testGroup $ \c g -> do
  conf <- conf' c
  deleteGroup g
  createConfiguration conf
  createGroup $ group' c g 0
  deleteGroup g >> deleteGroup g
  r <- runEitherT $ describeGroup g
  pure $ fmap isNothing r === Right True

prop_group_set_capacity = once . testGroup $ \c g -> do
  conf <- conf' c
  createConfiguration conf
  createGroup $ group' c g 0
  updateDesiredInstances g (DesiredInstances 1)
  r <- runEitherT $ describeGroup g
  pure $ (fmap . fmap) (desiredCapacity . groupResultCapacity) r === Right (Just $ DesiredInstances 1)

prop_update_tags = forAll ((,) <$> elements simpsons <*> elements boats) $ \(k, v) ->
  testGroup $ \c g -> do
    conf <- conf' c
    createConfiguration conf
    createGroup $ group' c g 0
    let tag = GroupTag (EC2Tag k v) Propagate
    updateTags g [tag]
    m <- runEitherT $ describeGroup g
    let r = fmap (elem tag . groupResultTags) . join $ rightToMaybe m
    pure $ r === Just True

prop_scale_in = once . testGroup $ \c g -> do
  conf <- conf' c
  createConfiguration conf
  createGroup $ group' c g 1

  -- Allow ec2 instance to start up
  liftIO . snooze $ seconds 45

  r <- describeOrFail g
  let is = scalingInstanceId <$> groupResultInstances r
  lockInstances g is
  locked <- describeOrFail g
  unlockInstances g is
  unlocked <- describeOrFail g
  let pro = fmap scalingInstanceProtected . groupResultInstances
  pure $
    (length is, pro locked, pro unlocked)
    ===
    (1, [ProtectedFromScaleIn], [NotProtectedFromScaleIn])

prop_update_min_max = once . testGroup $ \c g -> do
  let
    zeroCap = Capacity (MinInstances 0) (DesiredInstances 0) (MaxInstances 0)
  conf <- conf' c
  createConfiguration conf
  createGroup $ groupWithCapacity' c g zeroCap
  updateMinMaxInstances g (MinInstances 1) (MaxInstances 1)
  z <- describeOrFail g
  pure $ groupResultCapacity z ===
    Capacity (MinInstances 1) (DesiredInstances 1) (MaxInstances 1)

describeOrFail :: GroupName -> AWS GroupResult
describeOrFail g =
  eitherT
    (fail . T.unpack $ "GroupResultError [" <> renderGroupName g <> "]")
    (fromMaybeM (fail . T.unpack $ "Unable to find autoscaling group [" <> renderGroupName g <> "]"))
    (describeGroup g)

return []
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 5 })
