{-# LANGUAGE NoImplicitPrelude #-}
module Mismi.S3.Aws.Control (
    S3Action
  , Region (..)
  , runS3WithDefaults
  , runS3WithRegion
  , liftS3Action
  , liftAWSAction
  , epToRegion
  , regionToEp
  ) where

import           Data.ByteString hiding (unpack, find)

import           Mismi.Control
import           Mismi.S3.Aws.Data

import qualified Network.AWS.S3.Types as AWS

import           P

import           System.IO

-- | Specilised AwsAction for S3 operations
type S3Action = AWS

runS3WithDefaults :: S3Action a -> IO a
runS3WithDefaults = runS3WithRegion Sydney

runS3WithRegion :: Region -> S3Action a -> IO a
runS3WithRegion r = unsafeAWS . runAWS r

liftAWSAction :: AWS a -> S3Action a
liftAWSAction = id

liftS3Action :: S3Action a -> AWS a
liftS3Action = id

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
