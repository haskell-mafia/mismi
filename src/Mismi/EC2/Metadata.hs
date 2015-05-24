{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mismi.EC2.Metadata (
    MetadataError (..)
  , fetchMetadata
  , fetchInstanceId
  , fetchUserData
  , metadataErrorRender
  ) where

import           Control.Monad.Except
import           Control.Monad.Trans.Either

import           Data.ByteString.Char8      as BS
import           Data.Either.Combinators
import           Data.Text as T
import           Data.Text.Encoding as T

import           Mismi.EC2.Data as M

import           Network.AWS.EC2.Metadata as AWS
import           Network.HTTP.Conduit

import           P

import           System.IO


data MetadataError =
    MetadataHttpError HttpException
  | MetadataParseError Text


fetchMetadata :: AWS.Metadata -> EitherT MetadataError IO ByteString
fetchMetadata  metadata' = EitherT $ do
  m <- managerWithDefaultTimeout
  fmap (mapLeft MetadataHttpError) . runExceptT $ metadata m metadata'

fetchUserData :: EitherT MetadataError IO (Maybe M.UserData)
fetchUserData = EitherT $ do
  m <- managerWithDefaultTimeout
  fmap (mapLeft MetadataHttpError) . runExceptT $ userdata m >>=
     pure . fmap (M.UserData . T.decodeUtf8)

fetchInstanceId :: EitherT MetadataError IO M.InstanceId
fetchInstanceId =
  fetchMetadata AWS.InstanceId >>=
     hoistEither .
     maybeToRight (MetadataParseError "No lines returned from metadata service") .
     fmap M.InstanceId . listToMaybe . fmap T.decodeUtf8 . BS.lines

metadataErrorRender :: MetadataError -> Text
metadataErrorRender (MetadataHttpError e) = T.pack $ show e
metadataErrorRender (MetadataParseError t) = "Error parsing metadata " <> t

managerWithDefaultTimeout :: IO Manager
managerWithDefaultTimeout =
  newManager conduitManagerSettings {
    -- The default is normally 30 seconds
    managerResponseTimeout = Just 1000000 {- 1 second -}
  }
