{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Reliability.Mismi.S3.Commands where

import           Control.Monad.Catch
import           Control.Monad.IO.Class

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Disorder.Corpus

import           Mismi.S3

import           P

import qualified System.Directory as D
import           System.FilePath (combine)
import           System.IO
import           System.IO.Error
import           System.IO.Temp

import           Test.Reliability.Reliability
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

prop_sync = forAll (elements muppets) $ \m -> testAWS' $ \a b i -> do
  createFiles a m i
  syncWithMode OverwriteSync a b 10
  mapM_ (\e -> exists e >>= \e' -> when (e' == False) (throwM $ userError "Output files do not exist")) (files a m i)
  pure $ True === True

prop_list = forAll (elements muppets) $ \m -> testS3 $ \a i -> do
  createFiles a m i
  replicateM_ 100 (list a >>= \z -> when (length z /=  i) (throwM $ userError "List is not the same as original response"))
  pure $ True === True

prop_upload_single = forAll (elements muppets) $ \m -> testS3 $ \a i -> do
  withSystemTempDirectory "mismi" $ \p -> do
    liftIO $ D.createDirectoryIfMissing True p
    let f = combine p $ T.unpack m
    liftIO $ T.writeFile f "data"
    mapM_ (upload f) $ files a m i
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
