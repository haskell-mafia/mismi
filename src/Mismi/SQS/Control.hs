{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
module Mismi.SQS.Control (
    SQSAction
  , runSQSWithDefaults
  , runSQSWithCfg
  ) where

import qualified Aws
import qualified Aws.Sqs as SQS

import           Control.Monad.Reader hiding (forM)
import           Control.Monad.Trans.Resource (ResourceT)

import           Mismi.Control

import           Network.HTTP.Client (Manager)
import           Network.HTTP.Conduit (withManager)

import           P

import           System.IO

-- | Specilised AwsAction for S3 operations
type SQSAction = ReaderT (Aws.Configuration, SQS.SqsConfiguration Aws.NormalQuery, Manager) (ResourceT IO)

runSQSWithDefaults :: SQSAction a -> IO a
runSQSWithDefaults action = baseConfiguration' >>= \cfg -> runSQSWithCfg cfg action

runSQSWithCfg :: Aws.Configuration -> SQSAction a -> IO a
runSQSWithCfg cfg action =
  let scfg = Aws.defServiceConfig
  in withManager $ \m -> runReaderT action (cfg, scfg, m)
