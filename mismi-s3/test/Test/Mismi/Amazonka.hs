{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Mismi.Amazonka (
    createMultipart
  , sendMultipart
  , withMultipart
  , newMultipart
  ) where

import           Control.Monad.Catch
import           Control.Monad.Reader (ask)
import           Control.Monad.Trans.Resource

import           Data.Text as T
import           Data.Text.Encoding (encodeUtf8)

import           Control.Lens

import           Mismi.S3
import           Mismi.S3.Amazonka hiding (runAWS)
import           Mismi.S3.Internal

import           P

import           Test.Mismi.S3
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           X.Control.Monad.Trans.Either

createMultipart :: Address -> AWS Text
createMultipart a = do
  r <- send $ fencode' createMultipartUpload a & cmuServerSideEncryption .~ Just sse
  maybe (throwM . Invariant $ "Failed to create multipart upload") pure (r ^. cmursUploadId)

sendMultipart :: Text -> Address -> Int -> Text -> AWS ()
sendMultipart t a i ui = do
  let req = fencode' uploadPart a i ui (toBody $ encodeUtf8 t)
  void $ send req

withMultipart :: Testable a => (Address -> Text -> AWS a) -> Property
withMultipart f =
  testAWS $ do
    a <- newAddress
    awsBracket (createMultipart a) (abortMultipart' a) (f a)

newMultipart :: AWS (Address, Text)
newMultipart = do
  a <- newAddress
  r <- createMultipart a
  e <- ask
  void $ register (eitherT throwM pure . runAWS e $ abortMultipart' a r)
  void $ register (eitherT throwM pure . runAWS e $ listRecursively a >>= mapM_ delete >> delete a)
  pure $ (a, r)
