{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mismi.OpenSSL.Environment (
    X.Env
  , X.Region (..)
  , X.RegionError (..)
  , X.Debugging (..)
  , X.renderRegionError
  , X.getRegionFromEnv
  , X.getDebugging
  , X.setDebugging
  , discoverAWSEnv
  , discoverAWSEnvWithRegion
  , discoverAWSEnvRetry
  , discoverAWSEnvWithRegionRetry
  ) where

import           Control.Lens ((.~))
import           Control.Monad.Catch (Handler(..))
import           Control.Monad.Trans.AWS (Credentials(..), Region(..), Env)
import qualified Control.Monad.Trans.AWS as X
import           Control.Monad.Trans.Class (lift)
import           Control.Retry (RetryPolicyM, recovering, constantDelay, limitRetries)

import           Network.AWS.Env (envManager)
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.OpenSSL (opensslManagerSettings)
import           Network.HTTP.Client.OpenSSL (withOpenSSL)
import           OpenSSL.Session (context)

import           Mismi.Environment (RegionError (..), newMismiEnv)
import           Mismi.Environment (getDebugging, setDebugging, catchAuthError)
import qualified Mismi.Environment as X

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)

discoverAWSEnv :: EitherT RegionError IO Env
discoverAWSEnv =
  discoverAWSEnvRetry $ limitRetries 1 <> constantDelay 200000

discoverAWSEnvWithRegion :: Region -> IO Env
discoverAWSEnvWithRegion r =
  flip discoverAWSEnvWithRegionRetry r $ limitRetries 1 <> constantDelay 200000

discoverAWSEnvRetry :: RetryPolicyM IO -> EitherT RegionError IO Env
discoverAWSEnvRetry retry = do
  r <- X.getRegionFromEnv
  lift $ discoverAWSEnvWithRegionRetry retry r

discoverAWSEnvWithRegionRetry :: RetryPolicyM IO -> Region -> IO Env
discoverAWSEnvWithRegionRetry rpol r = do
  withOpenSSL $ pure ()
  d <- getDebugging
  e <- recovering rpol [(\_ -> Handler catchAuthError)] $ \_ -> newMismiEnv r Discover
  m <- newManager $ opensslManagerSettings context
  pure $ setDebugging d (e & envManager .~ m)
