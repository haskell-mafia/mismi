{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Reliability.Mismi.S3.Commands where

import           Control.Monad.Catch

import           Data.Text (Text)
import qualified Data.Text as T

import           Disorder.Corpus

import           Mismi.S3

import           P

import           System.IO
import           System.IO.Error

import           Test.Reliability.Reliability
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

prop_sync = forAll (elements muppets) $ \m -> testAWS' $ \a b i -> do
  createFiles a m i
  syncWithMode OverwriteSync a b 10
  mapM_ (\e -> exists e >>= \e' -> when (e' == False) (throwM $ userError "Output files do not exist")) (files a m i)
  pure $ True === True

createFiles :: Address -> Text -> Int -> AWS ()
createFiles prefix name n = do
  mapM_ (flip write "data") $ files prefix name n

files :: Address -> Text -> Int -> [Address]
files prefix name n =
  fmap (\i -> withKey (</> Key (name <> "-" <> (T.pack $ show i))) prefix) [1..n]

return []
tests :: IO Bool
tests =
  getMaxSuccess >>= testsN

testsN :: Int -> IO Bool
testsN n =
  $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = n })
