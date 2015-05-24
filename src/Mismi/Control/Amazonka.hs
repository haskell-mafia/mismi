{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mismi.Control.Amazonka (
    module X
  , AWSError (..)
  , runAWSDefaultRegion
  , awsBracket_
  , awsBracket
  , awsErrorRender
  , errorRender
  ) where

import           Control.Monad.Reader
import           Control.Monad.Trans.AWS as X hiding (AWSError)
import           Control.Monad.Trans.Either

import           Data.Bifunctor
import           Data.Text as T
import           Data.Text.Encoding as T

import           Mismi.Environment

import           Network.HTTP.Types.Status
import           P

import           System.IO

import           X.Exception.Catch


data AWSError =
    AWSRegionError RegionError
  | AWSRunError Error


runAWSDefaultRegion :: AWS a -> EitherT AWSError IO a
runAWSDefaultRegion a = do
  r <- EitherT . fmap (first AWSRegionError) $ getRegionFromEnv
  e <- liftIO $ getEnv r Discover
  EitherT . fmap (first AWSRunError) $ runAWST e a


awsBracket_ :: AWS a -> AWS c -> AWS b -> AWS b
awsBracket_ a b c =
  awsBracket a (const b) (const c)

awsBracket :: AWS a -> (a -> AWS c) -> (a -> AWS b) -> AWS b
awsBracket resource finalizer action = do
  e <- ask
  x <- liftIO $ bracketF
         (runAWST e resource)
         (\r -> case r of
             Left _ ->
               pure $ Right ()
             Right r' ->
               runAWST e (finalizer r') >>= \x -> pure $ case x of
                 Left err -> Left (Left err)
                 Right _ -> Right ())
         (\r -> case r of
             Left err ->
               pure $ Left err
             Right r' ->
               runAWST e (action r'))
  X.hoistEither x


awsErrorRender :: AWSError -> Text
awsErrorRender (AWSRegionError e) = regionErrorRender e
awsErrorRender (AWSRunError e) = errorRender e

errorRender :: Error -> Text
errorRender (HttpError e) =
  "Http error: " <> (T.pack $ show e)
errorRender (SerializerError a s) =
  "Serialization error. " <> T.intercalate ", " [
      "Abbreviation: " <> a
    , "Error: " <> T.pack s
    ]
errorRender (ServiceError a (Status sc sm) s) =
  "Service error: " <> T.intercalate ", " [
      "Abbreviation: " <> a
    , "Status code: " <> T.pack (show sc)
    , "Status message: " <> T.decodeUtf8 sm
    , "Error: " <> T.pack s
    ]
errorRender (Errors e) =
  T.unlines $ fmap errorRender e
