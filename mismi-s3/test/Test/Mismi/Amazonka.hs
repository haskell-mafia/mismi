{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Mismi.Amazonka (
    createMultipart
  , sendMultipart
  , withMultipart
  ) where

import           Data.Text
import           Data.Text.Encoding (encodeUtf8)

import           Control.Lens
import           Control.Monad.Trans.AWS

import           Mismi.S3.Amazonka
import           Mismi.S3.Control
import           Mismi.S3.Internal
import           Mismi.S3.Data
import           Mismi.Control.Amazonka

import           Test.Mismi.S3
import           Test.QuickCheck.Instances ()

import           Network.AWS.Data

import           Disorder.Core.IO

import           P

import           Test.QuickCheck

createMultipart :: Address -> AWS Text
createMultipart a = do
  r <- send $ f' createMultipartUpload a & cmuServerSideEncryption .~ Just sse
  maybe (fail "Failed to create multipart upload") pure (r ^. cmurUploadId)

sendMultipart :: Text -> Address -> Int -> Text -> AWS ()
sendMultipart t a i ui = do
  let req = f' (uploadPart (toBody $ encodeUtf8 t)) a i ui
  send_ req

withMultipart :: Testable a => (Address -> Text -> AWS a) -> Property
withMultipart f =
  property $ \t ->
    testIO . runS3WithDefaults . withToken t $ \a ->
      liftAWSAction $
        awsBracket (createMultipart a) (abortMultipart' a) (f a)
