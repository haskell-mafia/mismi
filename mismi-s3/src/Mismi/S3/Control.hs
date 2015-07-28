{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Mismi.S3.Control (
    S3Action
  , Region (..)
  , runS3WithDefaults
  , runS3WithRegion
  , runS3WithCfg
  , runS3WithManager
  , liftS3Action
  , liftAWSAction
  , epToRegion
  , regionToEp
  , retryHttp
  , retryHttpWithMessage
  , retryHttpWithPolicy
  , retryHttpWithPolicyOut
  ) where

import qualified Aws
import qualified Aws.S3 as S3
import           Aws.S3
import           Aws.Core (Protocol (..))

import           Data.Text (Text)
import qualified Data.Text as T

import           Data.ByteString hiding (unpack, find)

import           Control.Lens
import           Control.Monad.Catch (MonadMask, Handler (..))
import           Control.Monad.Reader hiding (forM)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Control.Monad.Trans.Either
import           Control.Retry

import           Mismi.Control
import           Mismi.S3.Internal

import           Network.HTTP.Client (HttpException, Manager)
import           Network.HTTP.Conduit (withManager)

import qualified Network.AWS.S3.Types as AWS

import           P

import           System.IO
import qualified System.IO as IO

-- | Specilised AwsAction for S3 operations
type S3Action = ReaderT (Aws.Configuration, S3.S3Configuration Aws.NormalQuery, Manager) (ResourceT IO)

runS3WithDefaults :: S3Action a -> IO a
runS3WithDefaults action =
  baseConfiguration' >>= \cfg -> runS3WithCfg cfg Sydney action

runS3WithRegion :: Region -> S3Action a -> IO a
runS3WithRegion r action =
  baseConfiguration' >>= \cfg -> runS3WithCfg cfg r action

runS3WithCfg :: Aws.Configuration -> Region -> S3Action a -> IO a
runS3WithCfg cfg r action =
  withManager (\m -> runS3WithManager cfg r m action)

runS3WithManager :: Aws.Configuration -> Region -> Manager -> S3Action a -> ResourceT IO a
runS3WithManager cfg r m action =
  let scfg = s3 HTTPS (regionToEp r) False
  in runReaderT action (cfg, scfg, m)

liftAWSAction :: AWS a -> S3Action a
liftAWSAction action = do
  (cfg, s3', mgr) <- ask
  r' <- maybe (fail $ "Invalid s3 endpoint [" <> (show $ s3Endpoint s3') <> "].") pure (epToRegion $ s3Endpoint s3')
  let x = Aws.credentials cfg
      ak = AccessKey $ Aws.accessKeyID x
      sk = SecretKey $ Aws.secretAccessKey x
  let c = maybe
            (FromKeys ak sk)
            (FromSession ak sk . SecurityToken)
            (Aws.iamToken x)
  env <- liftIO $ getEnvWithManager r' c mgr

  r <- liftIO . runEitherT $ runAWSWithEnv env action
  either throwAWSError pure r

liftS3Action :: S3Action a -> AWS a
liftS3Action action = do
  conf <- awskaConfig
  r <- view envRegion <$> ask
  m <- view envManager <$> ask
  liftIO . runResourceT $ runS3WithManager conf r m action

regionToEp :: Region -> ByteString
regionToEp r =
  _endpointHost $ endpoint s3service r

s3service :: (Service AWS.S3)
s3service = service

epToRegion :: ByteString -> Maybe Region
epToRegion bs = snd <$> find ((== bs) . fst) [
    (s3EndpointEu, Ireland)
  , (s3EndpointApNorthEast, Tokyo)
  , (s3EndpointApSouthEast2, Sydney)
  , (s3EndpointApSouthEast, Singapore)
  , (s3EndpointUsWest, NorthCalifornia)
  , (s3EndpointUsWest2, Oregon)
  , (s3EndpointUsClassic, NorthVirginia)
  ]

retryHttp :: (MonadMask m, MonadIO m) => Int -> m a -> m a
retryHttp i action =
  retryHttpWithPolicy (retryWithBackoff i) action

retryHttpWithMessage :: (MonadMask m, MonadIO m) => Int -> Text -> m a -> m a
retryHttpWithMessage i message action =
  retryHttpWithPolicyOut (retryWithBackoff i) (liftIO . IO.putStrLn $ "Retrying [" <> T.unpack message <> "].") action

retryHttpWithPolicy :: (MonadMask m, MonadIO m) => RetryPolicy -> m a -> m a
retryHttpWithPolicy policy action =
  retryHttpWithPolicyOut policy (return ()) action

retryHttpWithPolicyOut :: (MonadMask m, MonadIO m) => RetryPolicy -> m () -> m a -> m a
retryHttpWithPolicyOut policy out action =
  recovering
    policy
    ([const $ Handler (\(_ :: HttpException) -> out >> return True) ])
    action
