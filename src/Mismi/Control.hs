{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
module Mismi.Control (
    AwsAction
  , awsRequest
  , baseConfiguration'
  ) where

import qualified Aws
import           Aws.Aws
import           Aws.Core

import           Data.Text as T

import           Control.Monad.Catch
import           Control.Monad.Reader hiding (forM)
import           Control.Monad.Trans.Resource (ResourceT)

import           Network.HTTP.Client (Manager)

import           P

import           System.IO

type AwsAction r a = ReaderT (Aws.Configuration, Aws.ServiceConfiguration r Aws.NormalQuery, Manager) (ResourceT IO) a

-- TODO Be good to have the run* commands here, but that may not be possible wih type families for ServiceConfiguration

awsRequest :: Aws.Transaction r a => r -> AwsAction r a
awsRequest r = ReaderT $ \(cfg, scfg, mgr) -> Aws.pureAws cfg scfg mgr r


-- Snip from Aws library, this works around an issue where it throws exceptions if you don't
-- have a $HOME set (i.e. no shell) - this happens when you are running things as a daemon
-- where it should just fallback gracefully to env vars or instance-metadata.

baseConfiguration' :: (MonadCatch io, MonadIO io) => io Configuration
baseConfiguration' = liftIO $
    loadCredentialsDefault' >>=
      maybe (throwM $ NoCredentialsException "could not locate aws credentials") (\cr' -> return Configuration {
                      timeInfo = Timestamp
                    , credentials = cr'
                    , logger = defaultLog Warning
                    })

loadCredentialsDefault' :: (MonadCatch io, MonadIO io)  => io (Maybe Credentials)
loadCredentialsDefault' = do
  file <- liftM Just credentialsDefaultFile `catchIOError` (const . return) Nothing
  loadCredentialsFromEnvOrFileOrInstanceMetadata' file credentialsDefaultKey

loadCredentialsFromEnvOrFileOrInstanceMetadata' :: MonadIO io => Maybe FilePath -> T.Text -> io (Maybe Credentials)
loadCredentialsFromEnvOrFileOrInstanceMetadata' file key =
  loadCredentialsFromEnv >>=
    maybe loadCredentialsFromInstanceMetadata (return . return) >>=
    maybe (loadCredentialsFromFile' key file) (return . return)

loadCredentialsFromFile' :: MonadIO io => T.Text -> Maybe FilePath -> io (Maybe Credentials)
loadCredentialsFromFile' key =
 maybe (return Nothing) (flip loadCredentialsFromFile key)
