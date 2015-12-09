{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Mismi.Amazonka (
    sendMultipart
  , withMultipart
  , newMultipart
  ) where

import           Control.Monad.Catch
import           Control.Monad.Reader (ask)
import           Control.Monad.Trans.Resource

import           Data.Text as T
import           Data.Text.Encoding (encodeUtf8)

import           Mismi.S3
import qualified Mismi.S3.Amazonka as A
import           Mismi.S3.Internal

import           P

import           Test.Mismi.S3
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           X.Control.Monad.Trans.Either

sendMultipart :: Text -> Address -> Int -> Text -> AWS ()
sendMultipart t a i ui = do
  let req = f' A.uploadPart a i ui (A.toBody $ encodeUtf8 t)
  void $ A.send req

withMultipart :: Testable a => (Address -> Text -> AWS a) -> Property
withMultipart f =
  testAWS $ do
    a <- newAddress
    awsBracket (createMultipartUpload a) (abortMultipart' a) (f a)

newMultipart :: AWS (Address, Text)
newMultipart = do
  a <- newAddress
  r <- createMultipartUpload a
  e <- ask
  void $ register (eitherT throwM pure . runAWS e $ abortMultipart' a r)
  void $ register (eitherT throwM pure . runAWS e $ listRecursively a >>= mapM_ delete >> delete a)
  pure $ (a, r)
