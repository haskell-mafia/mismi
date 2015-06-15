{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Mismi.Control.Amazonka (
    module X
  , AWSError (..)
  , awskaConfig
  , runAWS
  , runAWSDefaultRegion
  , runAWSWithEnv
  , runAWSWithCreds
  , awsBracket_
  , awsBracket
  , awsErrorRender
  , errorRender
  , throwAWSError
  , throwError
  ) where

import           Aws.Aws
import           Aws.Core

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.AWS as X hiding (AWSError, Credentials, throwAWSError, getEnv)
import qualified Control.Monad.Trans.AWS as AWS
import           Control.Monad.Trans.Either

import           Data.IORef
import           Data.Bifunctor
import qualified Data.ByteString.Char8 as BS
import           Data.Text as T
import           Data.Text.Encoding as T

import           Mismi.Environment

import           Network.AWS.Data

import           Network.HTTP.Types.Status

import           P

import           System.IO
import           System.IO.Error
import           System.Environment

import           X.Control.Monad.Catch


data AWSError =
    AWSRegionError RegionError
  | AWSRunError Error

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

runAWS :: Region -> AWS a -> EitherT AWSError IO a
runAWS r a = do
  e <- liftIO $ AWS.getEnv r Discover
  token' <- liftIO $ lookupEnv "AWS_SECURITY_TOKEN"
  env <- maybe
           (pure e)
           (\token -> do
               auth <- withAuth (e ^. envAuth) (\ae -> right . Auth $ ae { _authToken = Just token })
               pure $ e & envAuth .~ auth)
           (SecurityToken . BS.pack <$> token')
  runAWSWithEnv env a

runAWSWithCreds :: Region -> AccessKey -> SecretKey -> Maybe SecurityToken -> Maybe UTCTime -> AWS a -> EitherT AWSError IO a
runAWSWithCreds r ak sk st ex a = do
  e <- liftIO $ AWS.getEnv r Discover
  let auth = Auth $ AuthEnv ak sk st ex
  let env = e & envAuth .~ auth
  runAWSWithEnv env a

runAWSDefaultRegion :: AWS a -> EitherT AWSError IO a
runAWSDefaultRegion a = do
  r <- EitherT . fmap (first AWSRegionError) $ getRegionFromEnv
  runAWS r a

runAWSWithEnv :: Env -> AWS a -> EitherT AWSError IO a
runAWSWithEnv e a =
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

throwAWSError :: (MonadThrow m) => AWSError -> m a
throwAWSError = \case
  AWSRegionError e -> fail' regionErrorRender e
  AWSRunError e -> throwError e

throwError :: (MonadThrow m) => Error -> m a
throwError = \case
  HttpError e -> throwM e
  e@(SerializerError _ _) -> fail' errorRender e
  e@(ServiceError _ _ _) -> fail' errorRender e
  Errors e -> maybe (fail "Error: Unknown") throwError $ listToMaybe e


fail' :: (MonadThrow m) => (e -> Text) -> e -> m a
fail' f =
  throwM . userError . unpack . f
