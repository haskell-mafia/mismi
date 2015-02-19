{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
module Mismi.Control (
    AwsAction
  , awsRequest
  ) where

import qualified Aws

import           Control.Monad.Reader
import           Control.Monad.Trans.Resource (ResourceT)

import           Network.HTTP.Client (Manager)

import           P

import           System.IO


type AwsAction r a = ReaderT (Aws.Configuration, Aws.ServiceConfiguration r Aws.NormalQuery, Manager) (ResourceT IO) a

-- TODO Be good to have the run* commands here, but that may not be possible wih type families for ServiceConfiguration

awsRequest :: Aws.Transaction r a => r -> AwsAction r a
awsRequest r = ReaderT $ \(cfg, scfg, mgr) -> Aws.pureAws cfg scfg mgr r
