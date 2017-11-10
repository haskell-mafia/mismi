{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Mismi.S3.Core.Arbitrary where

import qualified Data.List as L
import qualified Data.Text as T

import           Disorder.Corpus (simpsons, southpark)

import           Mismi.S3.Core.Data

import           P

import           Test.QuickCheck (Arbitrary (..), Gen)
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.Instances ()

import           System.FilePath (FilePath)

instance Arbitrary WriteMode where
  arbitrary = QC.elements [Fail, Overwrite]

instance Arbitrary Bucket where
  arbitrary = Bucket <$> QC.elements southpark

instance Arbitrary Address where
  arbitrary = QC.frequency [
      (9, Address <$> arbitrary <*> arbitrary)
    , (1, flip Address (Key "") <$> arbitrary)
    ]

instance Arbitrary Key where
  -- The max length of S3 Paths is 1024 - and we append some of them in the tests
  -- Unfortunately unicode characters aren't supported in the Haskell AWS library
  -- https://github.com/ambiata/vee/issues/7
  arbitrary =
    let genPath = QC.elements ["happy", "sad", ".", ":", "-"]
        path = do
          sep <- QC.elements ["-", "=", "#", ""]
          T.take 256 . T.intercalate "/" <$> QC.listOf1 (T.intercalate sep <$> QC.listOf1 genPath)
    in (Key . T.append "tests/") <$> path


fileNameSizePairs :: Int -> Gen [(FilePath, Int64)]
fileNameSizePairs len = do
  names <- QC.vectorOf len $ QC.elements simpsons
  lengths <- QC.vectorOf len $ QC.choose (1, 1000000000)
  pure $ L.zipWith3 zipper names [(0::Int) ..] lengths
  where
    zipper n i l = (n <> show i, l)
