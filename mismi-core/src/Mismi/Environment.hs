{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Mismi.Environment (
    Region (..)
  , RegionError (..)
  , getRegionFromEnv
  , regionErrorRender
  , discoverAWSEnv
  , envErrorRender
  ) where


import           Control.Monad.Catch
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           Control.Monad.IO.Class

import           Data.Text as T
import           Data.Typeable

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

getRegionFromEnv :: (MonadIO m, MonadThrow m) => m (Maybe Region)
getRegionFromEnv = do
  mr <- liftIO $ lookupEnv "AWS_DEFAULT_REGION"
  return $ maybe
    Nothing
    (either (throwM . RegionUnknown . T.pack) return . fromText . T.pack)
    mr

discoverAWSEnv :: IO (Either EnvError Env)
discoverAWSEnv = runEitherT $ do
  r <- EitherT . fmap (maybeToRight MissingRegion) $ getRegionFromEnv
  lift $ newEnv r Discover

regionErrorRender :: RegionError -> Text
regionErrorRender (RegionUnknown r) = "Unknown region: " <> r

envErrorRender :: EnvError -> Text
envErrorRender MissingRegion = "Environment variable AWS_DEFAULT_REGION was not found"
