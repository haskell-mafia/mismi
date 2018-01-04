{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}
module Mismi.EC2.Metadata (
    MetadataError (..)
  , fetchMetadata
  , fetchInstanceId
  , fetchUserData
  , metadataErrorRender
  ) where

import           Control.Monad.Catch (try)

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Mismi.EC2.Data

import qualified Network.AWS.EC2.Metadata as AWS
import           Network.HTTP.Conduit (HttpException)
import           Network.HTTP.Conduit (ManagerSettings (..))
import           Network.HTTP.Conduit (Manager, newManager)
import           Network.HTTP.Conduit (responseTimeoutMicro, tlsManagerSettings)


import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, pattern EitherT)
import           X.Control.Monad.Trans.Either (hoistEither)


data MetadataError =
    MetadataHttpError HttpException
  | MetadataParseError Text

fetchMetadata :: AWS.Metadata -> EitherT MetadataError IO ByteString
fetchMetadata  metadata' = EitherT $ do
  m <- managerWithDefaultTimeout
  fmap (first MetadataHttpError) . try $ AWS.metadata m metadata'

fetchUserData :: EitherT MetadataError IO (Maybe UserData)
fetchUserData = EitherT $ do
  m <- managerWithDefaultTimeout
  fmap (first MetadataHttpError) . try $ AWS.userdata m >>=
     pure . fmap (UserData . T.decodeUtf8)

fetchInstanceId :: EitherT MetadataError IO InstanceId
fetchInstanceId =
  fetchMetadata AWS.InstanceId >>=
     hoistEither .
     maybeToRight (MetadataParseError "No lines returned from metadata service") .
     fmap InstanceId . listToMaybe . fmap T.decodeUtf8 . BS.lines

metadataErrorRender :: MetadataError -> Text
metadataErrorRender (MetadataHttpError e) = T.pack $ show e
metadataErrorRender (MetadataParseError t) = "Error parsing metadata " <> t

managerWithDefaultTimeout :: IO Manager
managerWithDefaultTimeout =
#if MIN_VERSION_http_conduit(2,1,8)
  newManager tlsManagerSettings {
#else
  newManager conduitManagerSettings {
#endif
    -- The default is normally 30 seconds
    managerResponseTimeout = responseTimeoutMicro 1000000 {- 1 second -}
  }
