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
  , retryAction
  ) where

import           Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.UUID as U
import           Data.UUID.V4 as U

import           Control.Lens
import           Control.Monad.Trans.AWS hiding (AWSError)
import           Control.Monad.IO.Class (liftIO)

import           Mismi.S3
import           Mismi.S3.Commands hiding (listRecursively, listRecursively', delete)
import           Mismi.S3.Internal

import           Test.Mismi.S3
import           Test.QuickCheck.Instances ()

import           Network.AWS.Data

import           Disorder.Core.IO

import           P

import           System.FilePath
import           System.IO.Temp

import           Test.QuickCheck

createMultipart :: Address -> AWS Text
createMultipart a = retryAction $ do
  r <- send $ fencode' createMultipartUpload a & cmuServerSideEncryption .~ Just sse
  maybe (fail "Failed to create multipart upload") pure (r ^. cmurUploadId)

sendMultipart :: Text -> Address -> Int -> Text -> AWS ()
sendMultipart t a i ui = retryAction $ do
  let req = fencode' (uploadPart (toBody $ encodeUtf8 t)) a i ui
  send_ req

withAWS :: Testable a => (Address -> AWS a) -> Property
withAWS f =
  property $ \t ->
    testIO . unsafeAWS . runAWS Sydney . withAWSToken t $ \a ->
      f a

withAWS' :: Testable a => (Address -> Address -> AWS a) -> Property
withAWS' f =
  property $ \t t' ->
    testIO . unsafeAWS . runAWS Sydney . withAWSToken t $ \a ->
      withAWSToken t' $ \b ->
        f a b

withLocalAWS :: Testable a => (FilePath -> Address -> AWS a) -> Property
withLocalAWS f =
  property $ \t -> testIO . withSystemTempDirectory "mismi" $ \p ->
    unsafeAWS . runAWS Sydney . withAWSToken t $ \a ->
      f p a

withMultipart :: Testable a => (Address -> Text -> AWS a) -> Property
withMultipart f =
  property $ \t ->
    testIO . unsafeAWS . runAWS Sydney . withAWSToken t $ \a ->
      awsBracket (createMultipart a) (abortMultipart' a) (f a)

withAWSToken :: Token -> (Address -> AWS a) -> AWS a
withAWSToken t f = do
  b <- liftIO testBucket
  u <- liftIO $ T.pack . U.toString <$> U.nextRandom
  let a = Address b (Key . T.intercalate "/" $ ["mismi", u, unToken t])
  awsBracket_ (pure ()) ((retryAction . listRecursively) a >>= mapM_ delete >> delete a) (retryAction $ f a)

retryAction :: AWS a -> AWS a
retryAction = retryAWSAction (retryWithBackoff 5)
