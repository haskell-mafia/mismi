{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Mismi.S3.Commands where

import           Mismi.S3.Commands

import           P

import           System.IO

import           Test.QuickCheck

prop_chunks :: Property
prop_chunks =
  forAll (choose (0, 10000)) $ \size ->
    forAll (choose (0, 10000)) $ \chunk -> chunk < size ==>
      sum (snd' <$> calculateChunks size chunk) === size

snd' :: (Int, Int, Int) -> Int
snd' (_, b, _) = b

return []
tests :: IO Bool
tests = $quickCheckAll
