{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Mismi.S3.Internal where

import           Control.Monad.Catch (catchIOError)

import           Data.Text.IO as T

import           Disorder.Core.IO

import           Mismi.S3.Internal

import           P

import           System.Directory
import           System.IO
import           System.IO.Temp

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


prop_withFileSafe t = testIO . withSystemTempPath $ \f -> do
  f2 <- withFileSafe f $ \f2 -> f2 <$ T.writeFile f2 t
  t2 <- T.readFile f
  f2e <- doesFileExist f2
  pure $ (t, f2e) === (t2, False)

prop_withFileSafe_empty = testIO . withSystemTempPath $ \f -> do
  f2 <- withFileSafe f pure
  fmap not $ doesFileExist f2

prop_withFileSafe_error = testIO . withSystemTempPath $ \f -> do
  flip catchIOError (\_ -> pure ()) . withFileSafe f $ \_ -> fail ""
  fmap not $ doesFileExist f

prop_withFileSafe_error_exists t = testIO . withSystemTempPath $ \f -> do
  flip catchIOError (\_ -> pure ()) . withFileSafe f $ \f2 -> T.writeFile f2 t >> fail ""
  fmap not $ doesFileExist f


withSystemTempPath :: (FilePath -> IO t) -> IO t
withSystemTempPath run =
  withSystemTempDirectory "mismi-s3-file-safe" $ \dir -> run $ dir <> "/file"


return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 20 })
