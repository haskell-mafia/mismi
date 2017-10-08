{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Mismi.S3.Core.Arbitrary where

import qualified Data.List as DL
import           Data.Text as T

import           Disorder.Corpus

import           Mismi.S3.Core.Data

import           P

import           System.IO (FilePath)

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


instance Arbitrary WriteMode where
  arbitrary = elements [Fail, Overwrite]

instance Arbitrary Bucket where
  arbitrary = Bucket <$> elements southpark

instance Arbitrary Address where
  arbitrary = frequency [
      (9, Address <$> arbitrary <*> arbitrary)
    , (1, flip Address (Key "") <$> arbitrary)
    ]

instance Arbitrary Location where
  arbitrary = oneof [
      S3Location <$> arbitrary
    , LocalLocation <$> genFilePath
    ]

instance Arbitrary Key where
  -- The max length of S3 Paths is 1024 - and we append some of them in the tests
  -- Unfortunately unicode characters aren't supported in the Haskell AWS library
  -- https://github.com/ambiata/vee/issues/7
  arbitrary =
    let genPath = elements ["happy", "sad", ".", ":", "-"]
        path = do
          sep <- elements ["-", "=", "#", ""]
          T.take 256 . T.intercalate "/" <$> listOf1 (T.intercalate sep <$> listOf1 genPath)
    in (Key . append "tests/") <$> path


genFilePath :: Gen FilePath
genFilePath = do
  len <- choose (1, 10)
  DL.intercalate "/" . DL.take len <$> shuffle (simpsons <> southpark)
