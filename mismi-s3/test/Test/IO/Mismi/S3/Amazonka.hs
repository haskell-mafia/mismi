{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Mismi.S3.Amazonka where

import           Data.Text (Text)

import           Control.Lens
import           Control.Monad.Trans.AWS

import           Mismi.S3.Amazonka
import           Mismi.S3.Control
import           Mismi.S3.Data
import           Mismi.Control.Amazonka

import           Disorder.Core.IO

import           P

import           System.IO

import           Test.Mismi.S3
import           Test.Mismi.Amazonka
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


prop_list_multipart :: Property
prop_list_multipart = withMultipart $ \a i -> do
  sendMultipart "" a 1 i
  l <- listMultiparts (bucket a)
  pure $ (length $ f' l i) === 1

prop_abort_multipart :: Property
prop_abort_multipart = withMultipart $ \a i -> do
  sendMultipart "" a 1 i
  l <- listMultiparts (bucket a)
  forM_ (f' l i) $ abortMultipart (bucket a)
  r <- listMultiparts (bucket a)
  pure $ (length $ f' r i) === 0

f' :: [MultipartUpload] -> Text -> [MultipartUpload]
f' l t =
  filter (\p -> p ^. muUploadId == Just t) l

withMultipart :: Testable a => (Address -> Text -> AWS a) -> Property
withMultipart f =
  property $ \t ->
    testIO . runS3WithDefaults . withToken t $ \a ->
      liftAWSAction $ do
        awsBracket (createMultipart a) (abortMultipart' a) (f a)

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1 })
