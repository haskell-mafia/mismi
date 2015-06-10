{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Mismi.S3.Control where

import           Control.Monad.IO.Class
import           Control.Monad.Catch

import           Data.IORef
import           Data.Text

import           Disorder.Core.IO

import           Mismi.S3.Control

import           Network.HTTP.Client (HttpException (..))

import           Test.Mismi
import           Test.Mismi.S3
import           Test.Mismi.S3 ()

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

prop_retry :: Text -> Property
prop_retry t = t /= "" ==> testIO $ do
  ref <- newIORef ""
  let action = do
        let m = modifyIORef ref (<> t)
        z <- readIORef ref
        if (z == "")
          then m >> throwM ResponseTimeout
          else m
  retryHttp 2 action
  z <- readIORef ref
  pure $ z === t <> t

return []
tests :: IO Bool
tests = $quickCheckAll
