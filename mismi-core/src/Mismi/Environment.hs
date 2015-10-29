{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Mismi.Environment (
    Region (..)
  , RegionError (..)
  , Debugging (..)
  , getRegionFromEnv
  , getDebugging
  , setDebugging
  , renderRegionError
  , discoverAWSEnv
  , discoverAWSEnvWithRegion
  , discoverAWSEnvRetry
  , discoverAWSEnvWithRegionRetry
  ) where

import           Control.Lens ((.~), (&))
import           Control.Monad.Catch (MonadThrow(..), Handler(..))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.AWS (Credentials(..), Region(..))
import           Control.Monad.Trans.AWS (Env, newEnv, envLogger)
import           Control.Monad.Trans.AWS (Logger, LogLevel(..), newLogger)
import           Control.Monad.Trans.Class (lift)
import           Control.Retry (RetryPolicy, recovering, constantDelay, limitRetries)

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable)

import           Network.AWS.Auth (AuthError(..))
import           Network.AWS.Data (fromText)

import           P

import           System.Environment (lookupEnv)
import           System.IO (IO, stdout)

import           X.Control.Monad.Trans.Either (EitherT, left, right)

data RegionError =
    MissingRegion
  | UnknownRegion Text
  deriving (Eq, Show, Typeable)

data Debugging =
    DebugEnabled Logger
  | DebugDisabled


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
  maybe
    (return DebugDisabled)
    (\s ->
      case T.pack s of
        "true" ->
          return . DebugEnabled =<< newLogger Trace stdout
        "1" ->
          return . DebugEnabled =<< newLogger Trace stdout
        _ ->
          return DebugDisabled)
    d

setDebugging :: Debugging -> Env -> Env
setDebugging d e =
  case d of
    DebugEnabled lgr ->
      e & envLogger .~ lgr
    DebugDisabled ->
      e

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
  pure $ setDebugging d e
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
