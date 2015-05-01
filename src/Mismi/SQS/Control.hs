{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Mismi.SQS.Control (
    SQSAction
  , runSQSWithDefaults
  , runSQSWithCfg
  , loadRegionFromEnv
  , regionTo
  , sqsEndpointApSouthEast2
  ) where

import qualified Aws
import qualified Aws.Core
import qualified Aws.S3.Core
import qualified Aws.Sqs as SQS
import qualified Aws.Sqs.Core

import           Control.Monad.Reader hiding (forM)
import           Control.Monad.Trans.Resource (ResourceT)

import qualified Data.ByteString.Char8 as BS
import           Data.List (lookup)
import           Data.Maybe
import           Data.Text

import           Mismi.Control

import           Network.HTTP.Client (Manager)
import           Network.HTTP.Conduit (withManager)

import           P

import           System.Environment
import           System.IO

-- | Specilised AwsAction for SQS operations
type SQSAction = ReaderT (Aws.Configuration, SQS.SqsConfiguration Aws.NormalQuery, Manager) (ResourceT IO)

newtype Region = Region {
    unRegion :: Text
  } deriving (Eq, Show)

runSQSWithDefaults :: SQSAction a -> IO a
runSQSWithDefaults action = do
    cfg <- baseConfiguration'
    e <- loadRegionFromEnv >>= maybe (fail "No value found for AWS_DEFAULT_REGION.") (pure . Region)
    r <- (pure . regionTo . unRegion $ e) >>= maybe (fail "Failed to parse region from AWS_DEFAULT_REGION.") (pure)
    runSQSWithCfg cfg r action

runSQSWithCfg :: Aws.Configuration -> Aws.Sqs.Core.Endpoint -> SQSAction a -> IO a
runSQSWithCfg cfg endpoint  action =
  withManager $ \m -> do
    let scfg = SQS.sqs Aws.Core.HTTP endpoint False
    runReaderT action (cfg, scfg, m)

regionTo :: Text -> Maybe Aws.Sqs.Core.Endpoint
regionTo r = case r of
         "ap-northeast-1" -> Just Aws.Sqs.Core.sqsEndpointApNorthEast
         "ap-southeast-1" -> Just Aws.Sqs.Core.sqsEndpointApSouthEast
         "EU"             -> Just Aws.Sqs.Core.sqsEndpointEu
         "us-west-1"      -> Just Aws.Sqs.Core.sqsEndpointUsWest
         "us-west-2"      -> Just Aws.Sqs.Core.sqsEndpointUsWest2
         "ap-southeast-2" -> Just sqsEndpointApSouthEast2
         _ -> Nothing

sqsEndpointApSouthEast2 :: SQS.Endpoint
sqsEndpointApSouthEast2 =
  SQS.Endpoint (BS.pack "sqs.ap-southeast-2.amazonaws.com") Aws.S3.Core.locationApSouthEast2 [Aws.S3.Core.locationApSouthEast2]

loadRegionFromEnv :: IO (Maybe Text)
loadRegionFromEnv = liftIO $ do
  env <- getEnvironment
  let r = lookup "AWS_DEFAULT_REGION" env
  pure $ fmap pack r
