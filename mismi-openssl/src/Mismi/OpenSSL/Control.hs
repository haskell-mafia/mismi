{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Mismi.OpenSSL.Control (
    A.AWS
  , A.Error
  , A.AccessKey
  , A.SecretKey
  , A.SessionToken
  , A.Region (..)
  , X.withOpenSSL
  , C.runAWS
  , C.runAWST
  , C.runAWSTWith
  , C.rawRunAWS
  , runAWSTWithRegion
  , runAWSWithRegion
  , C.awsBracket
  , C.awsBracket_
  , C.newEnvFromCreds
  , C.renderError
  , C.setServiceRetry
  , C.setRetry
  , C.configureRetries
  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class

import qualified Mismi.Control as C
import           Mismi.OpenSSL.Environment

import           Network.AWS hiding (runAWS)
import qualified Network.AWS as A

import qualified Network.HTTP.Client.OpenSSL as X

import           P

import           System.IO

import           X.Control.Monad.Trans.Either

runAWSTWithRegion :: Region -> (Error -> e) -> EitherT e AWS a -> EitherT e IO a
runAWSTWithRegion r err action =
  C.runAWSTWith (runAWSWithRegion r) err action

runAWSWithRegion :: (MonadIO m, MonadCatch m) => Region -> AWS a -> EitherT Error m a
runAWSWithRegion r a = do
  e <- liftIO $ discoverAWSEnvWithRegion r
  C.runAWS e a
