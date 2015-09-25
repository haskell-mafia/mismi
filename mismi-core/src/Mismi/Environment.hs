{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Mismi.Environment (
    Region (..)
  , RegionError (..)
  , EnvError (..)
  , Debugging (..)
  , getRegionFromEnv
  , getDebugging
  , regionErrorRender
  , discoverAWSEnv
  , discoverAWSEnvRetry
  , envErrorRender
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
    RegionUnknown Text
  deriving (Eq, Typeable)

instance Exception RegionError

instance Show RegionError where
  show = T.unpack . regionErrorRender

data EnvError =
    MissingRegion
  deriving (Eq, Typeable)

instance Show EnvError where
  show = T.unpack . envErrorRender

data Debugging =
    DebugEnabled
  | DebugDisabled
  deriving (Eq, Show)

getRegionFromEnv :: (MonadIO m, MonadThrow m) => m (Maybe Region)
getRegionFromEnv = do
  mr <- liftIO $ lookupEnv "AWS_DEFAULT_REGION"
  return $ maybe
    Nothing
    (either (throwM . RegionUnknown . T.pack) return . fromText . T.pack)
    mr

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

discoverAWSEnv :: IO (Either EnvError Env)
discoverAWSEnv = discoverAWSEnvRetry $ limitRetries 1 <> constantDelay 200000

discoverAWSEnvRetry :: RetryPolicy -> IO (Either EnvError Env)
discoverAWSEnvRetry rpol = runEitherT $ do
  r <- EitherT . fmap (maybeToRight MissingRegion) $ getRegionFromEnv
  d <- getDebugging
  e <- lift $ recovering rpol [(\_ -> Handler catchAuthError)] $ newEnv r Discover
  case d of
    DebugEnabled -> do
      lgr <- lift $ newLogger Trace stdout
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

regionErrorRender :: RegionError -> Text
regionErrorRender (RegionUnknown r) = "Unknown region: " <> r

envErrorRender :: EnvError -> Text
envErrorRender MissingRegion = "Environment variable AWS_DEFAULT_REGION was not found"
