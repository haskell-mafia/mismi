{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Mismi.SQS.Control (
    SQSAction
  , runSQSWithDefaults
  , runSQSWithRegion
  , runSQSWithEndpoint
  , runSQSWithCfg
  , regionTo
  , regionEndpointOrFail
  , liftSQSAction
  ) where

import           Aws
import           Aws.Core
import           Aws.Sqs as SQS
import           Aws.Sqs.Core

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Resource (ResourceT)

import           Data.Maybe
import           Data.Text as T

import           Mismi.Control
import           Mismi.Environment
import           Mismi.SQS.Data

import           Network.AWS.Data (toText)
import           Network.AWS.Types
import           Network.HTTP.Client (Manager)
import           Network.HTTP.Conduit (withManager)

import           P

import           System.IO

liftSQSAction :: SQSAction a -> AWS a
liftSQSAction action = do
  conf <- awskaConfig
  r <- view envRegion <$> ask
  e <- maybe (fail . T.unpack $ "Region for SQS not supported" <> toText r) pure $ regionTo r
  liftIO $ runSQSWithCfg conf e action


-- | Specilised AwsAction for SQS operations
type SQSAction = ReaderT (Aws.Configuration, SQS.SqsConfiguration Aws.NormalQuery, Manager) (ResourceT IO)

runSQSWithDefaults :: SQSAction b -> IO b
runSQSWithDefaults action = do
  r <- getRegionFromEnv >>= either (fail . T.unpack . regionErrorRender) pure
  runSQSWithRegion r action

runSQSWithRegion :: Region -> SQSAction a -> IO a
runSQSWithRegion region' action = do
  e <- regionEndpointOrFail region'
  runSQSWithEndpoint e action

runSQSWithEndpoint :: Aws.Sqs.Core.Endpoint -> SQSAction a -> IO a
runSQSWithEndpoint endpoint' action = do
  cfg <- baseConfiguration'
  runSQSWithCfg cfg endpoint' action

runSQSWithCfg :: Aws.Configuration -> Aws.Sqs.Core.Endpoint -> SQSAction a -> IO a
runSQSWithCfg cfg endpoint' action =
  withManager $ \m -> do
    let scfg = SQS.sqs Aws.Core.HTTP endpoint' False
    runReaderT action (cfg, scfg, m)

regionTo :: Network.AWS.Types.Region -> Maybe Aws.Sqs.Core.Endpoint
regionTo r = case r of
  Network.AWS.Types.Tokyo             -> Just Aws.Sqs.Core.sqsEndpointApNorthEast
  Network.AWS.Types.Singapore         -> Just Aws.Sqs.Core.sqsEndpointApSouthEast
  Network.AWS.Types.NorthCalifornia   -> Just Aws.Sqs.Core.sqsEndpointUsWest
  Network.AWS.Types.Oregon            -> Just Aws.Sqs.Core.sqsEndpointUsWest2
  Network.AWS.Types.Sydney            -> Just sqsEndpointApSouthEast2
  Network.AWS.Types.Ireland           -> Just sqsEndpointEuWest1
  Network.AWS.Types.Frankfurt         -> Just sqsEndpointEuCentral1
  Network.AWS.Types.SaoPaulo          -> Just sqsEndpointSaEast1
  Network.AWS.Types.Beijing           -> Nothing
  Network.AWS.Types.NorthVirginia     -> Nothing
  Network.AWS.Types.GovCloud          -> Nothing
  Network.AWS.Types.GovCloudFIPS      -> Nothing

regionEndpointOrFail :: Region -> IO Aws.Sqs.Core.Endpoint
regionEndpointOrFail region' =
   maybe (fail . T.unpack $ "Region for SQS not supported" <> toText region') pure . regionTo $ region'
