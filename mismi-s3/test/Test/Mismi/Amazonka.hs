{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Mismi.Amazonka (
    createMultipart
  , sendMultipart
  , withMultipart
  , withAWS
  , withAWS'
  , withLocalAWS
  , withAWSToken
  ) where

import           Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.UUID as U
import           Data.UUID.V4 as U

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Class (liftIO)

import           Mismi.S3
import           Mismi.S3.Amazonka
import           Mismi.S3.Internal

import           Test.Mismi.S3
import           Test.QuickCheck.Instances ()

import           Disorder.Core.IO

import           P

import           System.FilePath
import           System.IO.Temp

import           Test.QuickCheck

createMultipart :: Address -> AWS Text
createMultipart a = do
  r <- send $ fencode' createMultipartUpload a & cmuServerSideEncryption .~ Just sse
  maybe (throwM . Invariant $ "Failed to create multipart upload") pure (r ^. cmursUploadId)

sendMultipart :: Text -> Address -> Int -> Text -> AWS ()
sendMultipart t a i ui = do
  let req = fencode' uploadPart a i ui (toBody $ encodeUtf8 t)
  void $ send req

withAWS :: Testable a => (Address -> AWS a) -> Property
withAWS f =
  property $ \t ->
    testAWS . withAWSToken t $ \a ->
      f a

withAWS' :: Testable a => (Address -> Address -> AWS a) -> Property
withAWS' f =
  property $ \t t' ->
    testAWS . withAWSToken t $ \a ->
      withAWSToken t' $ \b ->
        f a b

withLocalAWS :: Testable a => (FilePath -> Address -> AWS a) -> Property
withLocalAWS f =
  property $ \t -> testIO . withSystemTempDirectory "mismi" $ \p ->
    runAWSDefaultRegion . withAWSToken t $ \a ->
      f p a

withMultipart :: Testable a => (Address -> Text -> AWS a) -> Property
withMultipart f =
  property $ \t ->
    testAWS . withAWSToken t $ \a ->
      awsBracket (createMultipart a) (abortMultipart' a) (f a)

withAWSToken :: Token -> (Address -> AWS a) -> AWS a
withAWSToken t f = do
  b <- liftIO testBucket
  u <- liftIO $ T.pack . U.toString <$> U.nextRandom
  let a = Address b (Key . T.intercalate "/" $ ["mismi", u, unToken t])
  awsBracket_ (pure ()) (listRecursively a >>= mapM_ delete >> delete a) (f a)
