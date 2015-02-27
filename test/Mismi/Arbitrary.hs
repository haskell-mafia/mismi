{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Mismi.Arbitrary where

import           P

import           Mismi.S3.Data
import           Mismi.Test

import qualified Data.Text as T
import           Data.List ( take )



instance Arbitrary Key where
  -- The max length of S3 Paths is 1024 - and we append some of them in the tests
  arbitrary = parseKey . T.dropWhileEnd (== '/') . T.take 256 . T.intercalate "/" . fixPaths <$> listOf1 (T.intercalate "" <$> listOf1 genPath)
    where
      -- Unfortunately unicode characters aren't supported in the Haskell AWS library
      -- https://github.com/ambiata/vee/issues/7
      genPath = elements ["happy", "sad", ".", ":", "-"]

      -- We dont want the possibility of "../" coming up until we have a path at least 2 levels deep (?)
      --
      fixPaths :: [T.Text] -> [T.Text]
      fixPaths ("..":t:u:ts) = t:u:"..":ts
      fixPaths (t:"..":u:ts) = t:u:"..":ts
      fixPaths ts
        | length ts <= 2    = filter (/= "..") ts
        | otherwise         = ts
