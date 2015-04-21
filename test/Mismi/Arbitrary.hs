{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Mismi.Arbitrary where

import           Data.Text as T

import           Mismi.S3.Data
import           Mismi.Test

instance Arbitrary WriteMode where
  arbitrary = elements [Fail, Overwrite]

instance Arbitrary Address where
  arbitrary = Address (Bucket "ambiata-dev-view") <$> arbitrary

instance Arbitrary Key where
  -- The max length of S3 Paths is 1024 - and we append some of them in the tests
  -- Unfortunately unicode characters aren't supported in the Haskell AWS library
  -- https://github.com/ambiata/vee/issues/7
  arbitrary =
    let genPath = elements ["happy", "sad", ".", ":", "-"]
        path = T.dropWhileEnd ('/' ==) . T.take 128 . T.intercalate "/" <$> listOf1 (T.intercalate "" <$> listOf1 genPath)
    in (Key . append "tests/") <$> path
