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
  , liftS3
  , unsafeAWS
  ) where

import           Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.UUID as U
import           Data.UUID.V4 as U

import           Control.Lens
import           Control.Monad.Trans.AWS hiding (AWSError)
import           Control.Monad.Trans.Either
import           Control.Monad.IO.Class (liftIO)

import qualified Mismi.Control.Amazonka as A
import           Mismi.S3.Amazonka
import           Mismi.S3.Control
import           Mismi.S3.Internal
import           Mismi.S3.Data

import           Test.Mismi.S3
import           Test.QuickCheck.Instances ()

import           Network.AWS.Data

import           Disorder.Core.IO

import           P

import           System.FilePath
import           System.IO
import           System.IO.Temp

import           Test.QuickCheck

createMultipart :: Address -> AWS Text
createMultipart a = do
  r <- send $ f' createMultipartUpload a & cmuServerSideEncryption .~ Just sse
  maybe (fail "Failed to create multipart upload") pure (r ^. cmurUploadId)

sendMultipart :: Text -> Address -> Int -> Text -> AWS ()
sendMultipart t a i ui = do
  let req = f' (uploadPart (toBody $ encodeUtf8 t)) a i ui
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

liftS3 :: S3Action a -> AWS a
liftS3 a =
  liftS3Action $ retryHttpWithMessage 5 "https://github.com/ambiata/mismi/issues/125" a

withAWSToken :: Token -> (Address -> AWS a) -> AWS a
withAWSToken t f = do
  b <- liftIO testBucket
  u <- liftIO $ T.pack . U.toString <$> U.nextRandom
  let a = Address b (Key . T.intercalate "/" $ ["mismi", u, unToken t])
  awsBracket_ (pure ()) (listRecursively a >>= mapM_ delete >> delete a) (f a)

unsafeAWS :: EitherT A.AWSError IO a -> IO a
unsafeAWS =
  eitherT (fail . show . awsErrorRender) pure
