{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Mismi.S3.Amazonka where

import           Data.Text (Text)

import           Control.Lens

import           Mismi.S3.Amazonka
import           Mismi.S3.Data

import           Disorder.Core

import           P

import           System.IO

import           Test.Mismi.Amazonka
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

prop_list_multipart = withMultipart $ \a i -> do
  sendMultipart "" a 1 i
  l <- listMultiparts (bucket a)
  pure $ multipartExists i l

prop_abort_multipart = withMultipart $ \a i -> do
  sendMultipart "" a 1 i
  l <- listMultiparts (bucket a)
  forM_ (findMultiparts i l) $ abortMultipart (bucket a)
  r <- listMultiparts (bucket a)
  pure (neg $ multipartExists i r)

prop_list_parts = withMultipart $ \a i -> do
  sendMultipart "" a 1 i
  l2 <- listMultipartParts a i
  pure (length l2 === 1)

multipartExists :: Text -> [MultipartUpload] -> Property
multipartExists uploadId multiparts = count (findMultipart uploadId) multiparts === 1

findMultiparts :: Text -> [MultipartUpload] -> [MultipartUpload]
findMultiparts uploadId = filter (findMultipart uploadId)

findMultipart :: Text -> MultipartUpload -> Bool
findMultipart uploadId m = m ^. muUploadId == Just uploadId

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1 })
