{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Mismi.S3.Internal where

import           Mismi.S3.Internal

import           P

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

prop_chunks :: Property
prop_chunks =
  forAll (choose (1, 10000)) $ \size ->
    forAll (choose (1, size)) $ \chunk ->
      foldl' (+) 0 (snd' <$> calculateChunks size chunk) === size

prop_chunks_capped :: Property
prop_chunks_capped =
  forAll (choose (10, 10000)) $ \size ->
    forAll (choose (1, size)) $ \chunk ->
      forAll (choose (5, 15)) $ \cap ->
        length (calculateChunksCapped size chunk cap) <= cap

snd' :: (Int, Int, Int) -> Int
snd' (_, b, _) = b

return []
tests :: IO Bool
tests = $quickCheckAll
