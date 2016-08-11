
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Mismi.Autoscaling.Commands where

import           Disorder.Corpus

import           Mismi.Autoscaling.Core.Data
import           Mismi.Autoscaling.Commands

import           P

import           Test.Mismi.Autoscaling.Control
import           Test.Mismi.Autoscaling.Core.Arbitrary ()
import           Test.QuickCheck

import           X.Control.Monad.Trans.Either

prop_conf_create = once . testConf $ \cn -> do
  let conf = conf' cn
  createConfiguration conf
  r <- runEitherT $ describeConfiguration cn
  pure $ r === Right (Just conf)

prop_conf_delete = once . testConf $ \c -> do
  deleteConfiguration c
  createConfiguration $ conf' c
  deleteConfiguration c >> deleteConfiguration c
  r <- runEitherT $ describeConfiguration c
  pure $ r === Right Nothing

prop_conf_list = once . testConf $ \c ->do
  let conf = conf' c
  createConfiguration conf
  l <- runEitherT describeConfigurations
  pure $ fmap (elem conf) l === Right True

prop_group_create = once . testGroup $ \c g -> do
  createConfiguration $ conf' c
  createGroup $ group' c g 0
  r <- runEitherT $ describeGroup g
  pure $ (fmap . fmap) groupResultName r === Right (Just g)

prop_group_list = once . testGroup $ \c g -> do
  createConfiguration $ conf' c
  createGroup $ group' c g 0
  r <- runEitherT $ describeGroups
  let names = (fmap . fmap) groupResultName r
  pure $ fmap (elem g) names === Right True

prop_group_delete = once . testGroup $ \c g -> do
  deleteGroup g
  createConfiguration $ conf' c
  createGroup $ group' c g 0
  deleteGroup g >> deleteGroup g
  r <- runEitherT $ describeGroup g
  pure $ fmap isNothing r === Right True

prop_group_set_capacity = once . testGroup $ \c g -> do
  createConfiguration $ conf' c
  createGroup $ group' c g 0
  setCapacity g (DesiredInstances 1)
  r <- runEitherT $ describeGroup g
  pure $ (fmap . fmap) groupResultCapacity r === Right (Just $ DesiredInstances 1)

prop_update_tags = forAll ((,) <$> elements simpsons <*> elements boats) $ \(k, v) ->
  testGroup $ \c g -> do
    createConfiguration $ conf' c
    createGroup $ group' c g 0
    let tag = GroupTag (EC2Tag k v) Propagate
    updateTags g [tag]
    m <- runEitherT $ describeGroup g
    let r = fmap (elem tag . groupResultTags) . join $ rightToMaybe m
    pure $ r === Just True

return []
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 5 })
