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

prop_list :: Property
prop_list = forAll (elements muppets) $ \m -> testS3 $ \a i -> do
  createFiles a m i
  replicateM_ 100 (list a >>= \z -> when (length z /=  i) (throwM $ userError "List is not the same as original response"))
  pure $ True === True

createFiles :: Address -> Text -> Int -> S3Action ()
createFiles prefix name n = do
  mapM_ (\i ->
          flip write "data" $
            withKey (</> Key (name <> "-" <> (T.pack $ show i))) prefix
        ) [1..n]

return []
tests :: IO Bool
tests =
  getMaxSuccess >>= testsN

testsN :: Int -> IO Bool
testsN n =
  $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = n })
