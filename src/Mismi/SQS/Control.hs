{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Mismi.SQS.Control (
    SQSAction
  , runSQSWithCfg
  , regionTo
  ) where

import qualified Aws
import qualified Aws.Core
import qualified Aws.Sqs as SQS
import qualified Aws.Sqs.Core

import qualified Network.AWS.Types

import           Control.Monad.Reader hiding (forM)
import           Control.Monad.Trans.Resource (ResourceT)

import           Data.Maybe

import           Mismi.SQS.Data

import           Network.HTTP.Client (Manager)
import           Network.HTTP.Conduit (withManager)

import           P

import           System.IO

-- | Specilised AwsAction for SQS operations
type SQSAction = ReaderT (Aws.Configuration, SQS.SqsConfiguration Aws.NormalQuery, Manager) (ResourceT IO)

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
         _                                   -> Nothing
