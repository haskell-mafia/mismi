{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Mismi.Control (
    AwsAction
  , awsRequest
  , baseConfiguration'
  , runAction
  , awskaConfig
  , renderError
  ) where

import qualified Aws
import           Aws.Aws
import           Aws.Core

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Reader hiding (forM)
import           Control.Monad.Trans.Resource (ResourceT)
import           Control.Monad.Trans.AWS hiding (Credentials)
import           Control.Monad.Trans.Either

import           Data.IORef
import           Data.Text as T
import           Data.Text.Encoding as T

import           Network.HTTP.Client (Manager)
import           Network.HTTP.Types.Status

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

--- amazonka ---
runAction :: Region -> AWS a -> EitherT Error IO a
runAction r a = EitherT $ do
  e <- getEnv r Discover
  runAWST e a

renderError :: Error -> Text
renderError (HttpError e) =
  "Http error: " <> (T.pack $ show e)
renderError (SerializerError a s) =
  "Serialization error. " <> T.intercalate ", " [
      "Abbreviation: " <> a
    , "Error: " <> T.pack s
    ]
renderError (ServiceError a (Status sc sm) s) =
  "Service error: " <> T.intercalate ", " [
      "Abbreviation: " <> a
    , "Status code: " <> T.pack (show sc)
    , "Status message: " <> T.decodeUtf8 sm
    , "Error: " <> T.pack s
    ]
renderError (Errors e) =
  T.unlines $ fmap renderError e

awskaConfig :: AWS Configuration
awskaConfig = do
  env <- ask
  (AuthEnv (AccessKey ak) (SecretKey sak) st _) <- withAuth (env ^. envAuth) pure
  let st' = fmap (\(SecurityToken t') -> t') st
  v4sk <- liftIO $ newIORef []
  pure $ Configuration {
      timeInfo = Timestamp
    , credentials = Credentials ak sak v4sk st'
    , logger = defaultLog Warning
  }
