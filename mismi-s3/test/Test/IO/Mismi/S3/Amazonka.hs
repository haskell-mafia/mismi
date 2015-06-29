{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Mismi.S3.Amazonka where

import           Data.Text (Text)

import           Control.Lens hiding (elements)

import           Mismi.S3.Amazonka
import           Mismi.S3.Data
import qualified Mismi.S3.Commands as S3
import qualified Mismi.S3.Control as S3

import           Disorder.Core
import           Disorder.Corpus

import           P

import           System.IO

import           Test.Mismi.Amazonka
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

prop_exists = withMultipart $ \a _ -> do
  r <- exists a
  pure $ r /= True

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

prop_sync t = forAll ((,,) <$> elements muppets <*> elements simpsons <*> elements colours) $ \(m, s, c) -> withAWS $ \a -> do
  let source = withKey (</> Key m) a
      source' = withKey (\k -> k </> Key m </> Key s) a
      source'' = withKey (\k -> k </> Key m </> Key c) a
      dest = withKey (</> Key s) a
      dest' = withKey (\k -> k </> Key s </> Key s) a
      dest'' = withKey (\k -> k </> Key s </> Key c) a
  S3.liftS3Action $ S3.write source' t
  S3.liftS3Action $ S3.write source'' t
  syncWithMode OverwriteSync source dest 1
  e <- S3.liftS3Action $ S3.exists dest'
  e' <- S3.liftS3Action $ S3.exists dest''
  pure $ (e, e') === (True, True)



multipartExists :: Text -> [MultipartUpload] -> Property
multipartExists uploadId multiparts = count (findMultipart uploadId) multiparts === 1

findMultiparts :: Text -> [MultipartUpload] -> [MultipartUpload]
findMultiparts uploadId = filter (findMultipart uploadId)

findMultipart :: Text -> MultipartUpload -> Bool
findMultipart uploadId m = m ^. muUploadId == Just uploadId

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1 })
