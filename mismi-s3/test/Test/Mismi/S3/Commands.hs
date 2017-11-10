{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Mismi.S3.Commands where

import           Control.Lens ((.~))

import qualified Data.List as DL
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as DM
import           Data.Time.Clock (NominalDiffTime, addUTCTime, getCurrentTime)

import           Disorder.Core.IO (testIO)

import           Mismi.S3.Commands
import qualified Mismi.S3.Amazonka as A

import           P

import           System.IO

import           Test.Mismi.S3.Core.Arbitrary

import           Test.QuickCheck (Positive (..), Property, (===), quickCheckAll)
import qualified Test.QuickCheck as QC
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

prop_chunk_files_by_size :: Property
prop_chunk_files_by_size =
  QC.forAll (QC.choose (2, 10)) $ \ maxFilesPerChunk ->
  QC.forAll (QC.choose (10, 100)) $ \ fileCount ->
  QC.forAll (QC.choose (1000, 10000)) $ \ maxChunkSize ->
  QC.forAll (fileNameSizePairs fileCount) $ \ pairs ->
    let chunks = chunkFilesBySize maxFilesPerChunk maxChunkSize pairs
        chunkSizes = DL.map (multiChunkSum (DM.fromList pairs) . DL.map fst) chunks
    in
      DL.filter (> maxChunkSize) chunkSizes === []

  where
    multiChunkSum :: Map FilePath Int64 -> [FilePath] -> Int64
    multiChunkSum _ [] = 0
    multiChunkSum _ [_] = 0  -- Don't care about size of single file chunk.
    multiChunkSum sizes xs =
      sum $ mapMaybe (\ x -> DM.lookup x sizes) xs


return []
tests :: IO Bool
tests = $quickCheckAll
