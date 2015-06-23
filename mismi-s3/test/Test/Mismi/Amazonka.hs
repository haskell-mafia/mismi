{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Mismi.Amazonka (
    createMultipart
  , sendMultipart
  ) where

import           Control.Lens

import           Data.Text
import           Data.Text.Encoding (encodeUtf8)

import           Mismi.Control.Amazonka
import           Mismi.S3.Amazonka
import           Mismi.S3.Data
import           Mismi.S3.Internal

import           Network.AWS.Data

import           P

createMultipart :: Address -> AWS Text
createMultipart a = do
  r <- send $ f' createMultipartUpload a & cmuServerSideEncryption .~ Just sse
  maybe (fail "Failed to create multipart upload") pure (r ^. cmurUploadId)

sendMultipart :: Text -> Address -> Int -> Text -> AWS ()
sendMultipart t a i ui = do
  let req = f' (uploadPart (toBody $ encodeUtf8 t)) a i ui
  send_ $ req
