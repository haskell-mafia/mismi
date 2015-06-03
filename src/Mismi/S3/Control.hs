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
  , liftS3Action
  , liftAWSAction
  , epToRegion
  , regionToEp
  ) where

import qualified Aws
import qualified Aws.S3 as S3
import           Aws.S3
import           Aws.Core (Protocol (..))

import           Data.ByteString hiding (unpack, find)
import           Data.Text (unpack)

import           Control.Lens
import           Control.Monad.Reader hiding (forM)
import           Control.Monad.Trans.Resource (ResourceT)
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Either

import           Mismi.Control

import           Network.HTTP.Client (Manager)
import           Network.HTTP.Conduit (withManager)

import qualified Network.AWS.S3.Types as AWS

import           P

import           System.IO

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
  let scfg = s3 HTTPS (regionToEp r) False
  in withManager (\m -> runReaderT action (cfg, scfg, m))

liftAWSAction :: AWS a -> S3Action a
liftAWSAction action = do
  (_, s3', _) <- ask
  r' <- maybe (fail $ "Invalid s3 endpoint [" <> (show $ s3Endpoint s3') <> "].") pure (epToRegion $ s3Endpoint s3')
  r <- liftIO . runEitherT $ runAction r' action
  either (fail . unpack . renderError ) (pure) r

liftS3Action :: S3Action a -> AWS a
liftS3Action action = do
  conf <- awskaConfig
  r <- view envRegion <$> ask
  liftIO $ runS3WithCfg conf r action

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
