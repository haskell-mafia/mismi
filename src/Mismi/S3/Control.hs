{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
module Mismi.S3.Control (
    S3Action
  , runS3WithDefaults
  , runS3WithCfg
  ) where

import qualified Aws
import           Aws.Aws
import           Aws.Core
import qualified Aws.S3 as S3

import           Data.Text as T

import           Control.Monad.Catch
import           Control.Monad.Reader hiding (forM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource (ResourceT)

import           Network.HTTP.Client (Manager)
import           Network.HTTP.Conduit (withManager)

import           P

import           System.IO


-- | Specilised AwsAction for S3 operations
type S3Action = ReaderT (Aws.Configuration, S3.S3Configuration Aws.NormalQuery, Manager) (ResourceT IO)

runS3WithDefaults :: S3Action a -> IO a
runS3WithDefaults action = do
  baseConfiguration' >>= \cfg -> runS3WithCfg cfg action

runS3WithCfg :: Aws.Configuration -> S3Action a -> IO a
runS3WithCfg cfg action =
  let scfg = Aws.defServiceConfig
  in withManager (\m -> runReaderT action (cfg, scfg, m))

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
    maybe (loadCredentialsFromInstanceMetadata) (return . return) >>=
    maybe (loadCredentialsFromFile' key file) (return . return)

loadCredentialsFromFile' :: MonadIO io => T.Text -> Maybe FilePath -> io (Maybe Credentials)
loadCredentialsFromFile' key =
 maybe (return Nothing) (flip loadCredentialsFromFile key)
