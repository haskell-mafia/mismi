{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Mismi.S3.CommandsTest where

import qualified Aws.S3 as S3

import           Data.List (sort)

import           Mismi.S3.Control
import           Mismi.S3.Commands
import           Mismi.S3.Data
import           Mismi.Test
import           Mismi.Test.S3


prop_getTextUtf8_empty :: Key -> Property
prop_getTextUtf8_empty k = ioProperty $ do
  bucket' <- testBucket
  t <- runS3WithDefaults . getTextUtf8 $ Address bucket' k
  pure $ t === Nothing

prop_getTextUtf8 :: KeyTmp -> Property
prop_getTextUtf8 k = ioProperty $ do
  bucket' <- testBucket
  runS3WithDefaults . withTmpKey k $ do
    t' <- getTextUtf8 $ Address bucket' (tmpPath k)
    pure $ t' === Just (tmpBody k)

prop_getObjects_empty :: KeyTmp -> Property
prop_getObjects_empty p = ioProperty $ do
  bucket' <- testBucket
  objs <- runS3WithDefaults . getObjects $ Address bucket' (tmpPath p)
  pure $ fmap S3.objectKey objs === []

prop_getObjects :: KeyTmp -> Key -> Key -> Property
prop_getObjects prefix p1 p2 = p1 /= p2 ==> ioProperty $ do
  bucket' <- testBucket
  runS3WithDefaults .
    withTmpKey (prefix <//> p1) .
    withTmpKey (prefix <//> p2) $ do
      objs <- getObjects $ Address bucket' (tmpPath prefix)
      pure $ on (===) sort (fmap S3.objectKey objs) (fmap (showKey . tmpPath) [prefix <//> p1, prefix <//> p2])


return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
