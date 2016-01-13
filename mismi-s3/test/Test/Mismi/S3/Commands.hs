{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Mismi.S3.Commands where

import           Control.Lens ((.~))

import           Data.Time.Clock

import           Disorder.Core.IO

import           Mismi.S3.Commands
import qualified Mismi.S3.Amazonka as A

import           P

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

prop_filter_old :: Positive NominalDiffTime -> Property
prop_filter_old (Positive i) = testIO $ do
  n <- getCurrentTime
  let t = addUTCTime ((-1 * ((60 * 60 * 24 * 7) + i)) :: NominalDiffTime) n
      r = filterOld n $ A.multipartUpload & A.muInitiated .~ Just t
  pure $ r === True

prop_filter_failure :: Property
prop_filter_failure = testIO $ do
  n <- getCurrentTime
  let r = filterOld n $ A.multipartUpload & A.muInitiated .~ Just n
  pure $ r === False

return []
tests :: IO Bool
tests = $quickCheckAll
