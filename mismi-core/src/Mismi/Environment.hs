{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Mismi.Environment (
    Region (..)
  , RegionError (..)
  , Debugging (..)
  , getRegionFromEnv
  , getDebugging
  , renderRegionError
  , discoverAWSEnv
  , discoverAWSEnvWithRegion
  , discoverAWSEnvRetry
  , discoverAWSEnvWithRegionRetry
  ) where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           Control.Monad.IO.Class
import           Control.Retry

import           Data.Text as T
import           Data.Typeable

import           Network.AWS.Auth
import           Network.AWS.Data

import           P

import           System.Environment
import           System.IO

data RegionError =
    MissingRegion
  | UnknownRegion Text
  deriving (Eq, Typeable)

data Debugging =
    DebugEnabled
  | DebugDisabled
  deriving (Eq, Show)


getRegionFromEnv :: (MonadIO m, MonadThrow m) => EitherT RegionError m Region
getRegionFromEnv = do
  mr <- liftIO $ lookupEnv "AWS_DEFAULT_REGION"
  case mr of
    Nothing ->
      left MissingRegion
    Just a ->
      case fromText $ T.pack a of
        Left e ->
          left $ UnknownRegion (T.pack e)
        Right r ->
          right r

getDebugging :: MonadIO m => m Debugging
getDebugging = do
  d <- liftIO $ lookupEnv "AWS_DEBUG"
  return $ maybe
    DebugDisabled
    (\s ->
      case T.pack s of
        "true" ->
          DebugEnabled
        "1" ->
          DebugEnabled
        _ ->
          DebugDisabled)
    d

discoverAWSEnv :: EitherT RegionError IO Env
discoverAWSEnv =
  discoverAWSEnvRetry $ limitRetries 1 <> constantDelay 200000

discoverAWSEnvWithRegion :: Region -> IO Env
discoverAWSEnvWithRegion r =
  flip discoverAWSEnvWithRegionRetry r $ limitRetries 1 <> constantDelay 200000

discoverAWSEnvRetry :: RetryPolicy -> EitherT RegionError IO Env
discoverAWSEnvRetry retry = do
  r <- getRegionFromEnv
  lift $ discoverAWSEnvWithRegionRetry retry r

discoverAWSEnvWithRegionRetry :: RetryPolicy -> Region -> IO Env
discoverAWSEnvWithRegionRetry rpol r = do
  d <- getDebugging
  e <- recovering rpol [(\_ -> Handler catchAuthError)] $ newEnv r Discover
  case d of
    DebugEnabled -> do
      lgr <- newLogger Trace stdout
      pure $ e & envLogger .~ lgr
    DebugDisabled ->
      pure e
  where
    catchAuthError :: AuthError -> IO Bool
    -- MDS sometimes has transient failures.
    catchAuthError (RetrievalError _)   = pure True
    -- 'MissingFileError' is rethrown from 'getAuth' in
    -- 'Discover' mode if 'isEC2' (which queries the MDS) returns
    -- 'False'.
    -- FIXME(sio): fix this upstream so we can distinguish between
    -- legit 'MissingFileError's and MDS failures.
    catchAuthError (MissingFileError _) = pure True
    -- Everything else is unlikely to be transient.
    catchAuthError _                    = pure False

renderRegionError :: RegionError -> Text
renderRegionError e =
  case e of
    UnknownRegion r ->
      "Unknown region: " <> r
    MissingRegion ->
      "Environment variable AWS_DEFAULT_REGION was not found"
