{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Mismi.S3.Amazonka where

import           Control.Lens

import           Data.Time.Clock

import           Mismi.S3.Amazonka

import           Disorder.Core.IO

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

return []
tests :: IO Bool
tests = $quickCheckAll
