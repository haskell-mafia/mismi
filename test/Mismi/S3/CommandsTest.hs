{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Mismi.S3.CommandsTest where

import qualified Aws.S3 as S3

import           Control.Monad.Catch (catch)
import           Control.Exception (SomeException)

import           Data.Bool
import           Data.List (sort)
import           Data.Text as T

import           Mismi.S3.Control
import           Mismi.S3.Commands
import           Mismi.S3.Data
import           Mismi.Test
import           Mismi.Test.S3

import           Test.QuickCheck.Monadic

import Prelude (putStrLn)

prop_exists :: Address -> Property
prop_exists a = monadicIO $ do
  r <- run $ do
    runS3WithDefaults . withAddress a $ do
      write a ""
      exists a
  stop $ r === True

prop_exists_empty :: Address -> Property
prop_exists_empty a = monadicIO $ do
  r <- run $ do
    runS3WithDefaults . withAddress a $ do
      exists a
  stop $ r === False

prop_delete :: Address -> Property
prop_delete a = monadicIO $ do
  r <- run $ do
    runS3WithDefaults . withAddress a $ do
      write a ""
      x <- exists a
      delete a
      y <- exists a
      pure $ (x, y)
  stop $ r == (True, False)

prop_delete_empty :: Address -> Property
prop_delete_empty a = monadicIO $ do
    r <- run $ (do
      runS3WithDefaults . withAddress a $ do
        delete a
        pure $ True)
         `catch`
         (\(e :: SomeException) -> pure $ False)
    stop $ r === True

prop_read_write :: Address -> Text -> Property
prop_read_write a d = monadicIO $ do
    r <- run $ do
      runS3WithDefaults . withAddress a $ do
        write a d
        read a
    stop $ r === Just d

prop_write_failure :: Address -> Text -> Property
prop_write_failure a d = monadicIO $ do
    r <- run $ (do
      runS3WithDefaults . withAddress a $ do
        write a d
        write a d
        pure $ False)
         `catch`
         (\(e :: SomeException) -> pure $ True)
    stop $ r === True

prop_read_empty :: Key -> Property
prop_read_empty k = ioProperty $ do
  bucket' <- testBucket
  t <- runS3WithDefaults . read $ Address bucket' k
  pure $ t === Nothing

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
    withTmpKey (prefix <//> p2 <//> p1 ) .
    withTmpKey (prefix <//> p2 <//> p2) $ do
      objs <- getObjects $ Address bucket' (tmpPath prefix)
      pure $ on (===) sort (fmap S3.objectKey objs) (fmap (unKey . tmpPath) [prefix <//> p1, prefix <//> p2 <//> p1, prefix <//> p2 <//> p2])

prop_list :: Address -> Property
prop_list a = monadicIO $ do
  r <- run $ do
    runS3WithDefaults . withAddress a $ do
      write a ""
      listRecursively (a { key = dirname $ key a })
  stop $ a `elem` r == True

prop_dirname :: Address -> Text -> Property
prop_dirname a t =
  let k = Key t in
  (T.all (/= '/') t) ==>
    (dirname $ key a </> k) === key a

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
