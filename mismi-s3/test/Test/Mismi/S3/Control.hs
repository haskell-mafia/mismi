{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Mismi.S3.Control where

import           Control.Monad.Trans.AWS
import           Control.Monad.IO.Class

import           Data.IORef
import           Data.Text

import           Disorder.Core.IO

import           Mismi.S3.Aws.Control

import           P

import           System.IO

import           Test.Mismi.S3
import           Test.Mismi.S3 ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


prop_endpoint :: Region -> Property
prop_endpoint r =
  (epToRegion $ regionToEp r) === Just r

prop_liftAWS :: Text -> Property
prop_liftAWS t = testIO . runS3WithDefaults $ do
  ref <- liftIO $ newIORef ""
  liftAWSAction (liftIO $ modifyIORef ref (<> t) )
  f <- liftIO $ readIORef ref
  pure $ f === t

prop_liftS3 :: Region -> Text -> Property
prop_liftS3 r t = testAWS r $ do
  ref <- liftIO $ newIORef ""
  liftS3Action (liftIO $ modifyIORef ref (<> t) )
  f <- liftIO $ readIORef ref
  pure $ f === t


return []
tests :: IO Bool
tests = $quickCheckAll
