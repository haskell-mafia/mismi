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

      -- We dont want the possibility of "../" coming up at the root level
      -- <https://github.com/ambiata/mismi/commit/e9beff0162ef26eec0fc1b368eb23731160ffdc3#commitcomment-9949978>
      fixPaths :: [T.Text] -> [T.Text]
      fixPaths ("..":t:ts) = t:"..":ts
      fixPaths ts
        | length ts < 2     = filter (/= "..") ts
        | otherwise         = ts
