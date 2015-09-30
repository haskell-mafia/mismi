{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Mismi.S3.Commands where

import           Control.Lens

import           Data.Time.Clock

import           Disorder.Core.IO

import           Mismi.S3.Internal
import           Mismi.S3.Commands
import           Mismi.S3.Amazonka

import           P

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

prop_filter_old :: Positive NominalDiffTime -> Property
prop_filter_old (Positive i) = testIO $ do
  n <- getCurrentTime
  let t = addUTCTime ((-1 * ((60 * 60 * 24 * 7) + i)) :: NominalDiffTime) n
      r = filterOld n $ multipartUpload & muInitiated .~ Just t
  pure $ r === True

prop_filter_failure :: Property
prop_filter_failure = testIO $ do
  n <- getCurrentTime
  let r = filterOld n $ multipartUpload & muInitiated .~ Just n
  pure $ r === False

prop_chunks :: Property
prop_chunks =
  forAll (choose (1, 10000)) $ \size ->
    forAll (choose (1, size)) $ \chunk ->
      foldl' (+) 0 (snd' <$> calculateChunks size chunk) === size

snd' :: (Int, Int, Int) -> Int
snd' (_, b, _) = b

return []
tests :: IO Bool
tests = $quickCheckAll
