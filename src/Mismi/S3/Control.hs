{-# LANGUAGE NoImplicitPrelude #-}
module Mismi.S3.Control (
    S3Action
  , runS3WithDefaults
  , runS3WithCfg
  ) where

import qualified Aws
import qualified Aws.S3 as S3

import           Control.Monad.Reader
import           Control.Monad.Trans.Resource (ResourceT)

import           Network.HTTP.Client (Manager)
import           Network.HTTP.Conduit (withManager)

import           System.IO


-- | Specilised AwsAction for S3 operations
type S3Action = ReaderT (Aws.Configuration, S3.S3Configuration Aws.NormalQuery, Manager) (ResourceT IO)

runS3WithDefaults :: S3Action a -> IO a
runS3WithDefaults action = do
  Aws.baseConfiguration >>= \cfg -> runS3WithCfg cfg action

runS3WithCfg :: Aws.Configuration -> S3Action a -> IO a
runS3WithCfg cfg action =
  let scfg = Aws.defServiceConfig
  in withManager (\m -> runReaderT action (cfg, scfg, m))
