{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Mismi.Environment (
    Region (..)
  , RegionError (..)
  , getRegionFromEnv
  , regionErrorRender
  ) where


import           Control.Monad.Catch
import           Control.Monad.Trans.AWS
import           Control.Monad.IO.Class

import           Data.Text as T
import           Data.Typeable

import           Network.AWS.Data

import           P

import           System.Environment


data RegionError =
    RegionUnknown Text
  deriving (Eq, Typeable)

instance Exception RegionError

instance Show RegionError where
  show = T.unpack . regionErrorRender

getRegionFromEnv :: (MonadIO m, MonadThrow m) => m (Maybe Region)
getRegionFromEnv = do
  mr <- liftIO $ lookupEnv "AWS_DEFAULT_REGION"
  return $ maybe
    Nothing
    (either (throwM . RegionUnknown . T.pack) return . fromText . T.pack)
    mr

regionErrorRender :: RegionError -> Text
regionErrorRender (RegionUnknown r) = "Unknown region: " <> r
