{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Mismi.Arbitrary where

import           Data.Text as T

import           Disorder.Corpus

import           Mismi.S3.Data
import           Mismi.SQS.Data

import           Test.Mismi

instance Arbitrary WriteMode where
  arbitrary = elements [Fail, Overwrite]

instance Arbitrary Bucket where
  arbitrary = Bucket <$> elements southpark

instance Arbitrary Address where
  arbitrary = frequency [
      (9, Address <$> arbitrary <*> arbitrary)
    , (1, flip Address (Key "") <$> arbitrary)
    ]

instance Arbitrary Key where
  -- The max length of S3 Paths is 1024 - and we append some of them in the tests
  -- Unfortunately unicode characters aren't supported in the Haskell AWS library
  -- https://github.com/ambiata/vee/issues/7
  arbitrary =
    let genPath = elements ["happy", "sad", ".", ":", "-"]
        path = T.take 256 . T.intercalate "/" <$> listOf1 (T.intercalate "" <$> listOf1 genPath)
    in (Key . append "tests/") <$> path

instance Arbitrary QueueName where
  arbitrary = (QueueName . T.pack) <$> sized (\n -> vectorOf (max 80 n) (oneof [
      choose ('a', 'z')
    , choose ('0', '9')
    , pure '-'
    , pure '_'
    ]))
