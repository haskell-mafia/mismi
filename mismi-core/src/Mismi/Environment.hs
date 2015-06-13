{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mismi.Environment (
    Region (..)
  , RegionError (..)
  , getRegionFromEnv
  , regionErrorRender
  ) where


import           Control.Monad.Trans.AWS

import           Data.Bifunctor
import           Data.Text as T

import           Network.AWS.Data

import           P

import           System.Environment
import           System.IO


data RegionError =
    RegionMissing
  | RegionUnknown Text
  deriving (Eq, Show)


getRegionFromEnv :: IO (Either RegionError Region)
getRegionFromEnv =
  lookupEnv "AWS_DEFAULT_REGION" >>= \r ->
    pure .
      (=<<) (\r' -> first (const . RegionUnknown $ T.pack r') . fromText . T.pack $ r')
      . maybeToRight RegionMissing
      $ r


regionErrorRender :: RegionError -> Text
regionErrorRender RegionMissing = "Could not find the required $AWS_DEFAULT_REGION environment variable"
regionErrorRender (RegionUnknown r) = "Unknown region: " <> r
