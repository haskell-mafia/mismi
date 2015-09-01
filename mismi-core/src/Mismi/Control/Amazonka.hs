{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Mismi.Control.Amazonka (
    module X
  , AWSError (..)
  , foldAWSError
  , runAWS
  , runAWSDefaultRegion
  , runAWSWithEnv
  , runAWSWithCreds
  , getEnvWithManager
  , awsBracket_
  , awsBracket
  , awsErrorRender
  , errorRender
  , throwAWSError
  , throwError
  , hoistAWSError
  , liftAWSError
  , unsafeAWS
  ) where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.AWS as X hiding (AWSError, Credentials, throwAWSError, getEnv)
import qualified Control.Monad.Trans.AWS as AWS
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Except (runExceptT)

import           Data.Bifunctor
import qualified Data.ByteString.Char8 as BS
import           Data.Text as T
import           Data.Text.Encoding as T

import           Mismi.Environment

import           Network.HTTP.Client (Manager)
import           Network.HTTP.Client.Internal (mResponseTimeout)
import           Network.HTTP.Types.Status

import           P
import           Prelude as PreludeUnsafe (error)

import           System.IO
import           System.IO.Error
import           System.Environment

import           X.Control.Monad.Catch


data AWSError =
    AWSRegionError RegionError
  | AWSRunError Error

foldAWSError :: (RegionError -> a) -> (Error -> a) -> AWSError -> a
foldAWSError r e = \case
  AWSRegionError r' -> r r'
  AWSRunError e' -> e e'


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

runAWSWithCreds :: Region -> AccessKey -> SecretKey -> Maybe SecurityToken -> AWS a -> EitherT AWSError IO a
runAWSWithCreds r ak sk st a = do
  e <- liftIO . AWS.getEnv r $ case st of
    Nothing ->
      FromKeys ak sk
    Just st' ->
      FromSession ak sk st'
  runAWSWithEnv e a

runAWSDefaultRegion :: AWS a -> EitherT AWSError IO a
runAWSDefaultRegion a = do
  r <- EitherT . fmap (first AWSRegionError) $ getRegionFromEnv
  e <- liftIO $ AWS.getEnv r Discover
  runAWSWithEnv e a

runAWSWithEnv :: Env -> AWS a -> EitherT AWSError IO a
runAWSWithEnv e a =
  let e' = over envManager (\m -> m { mResponseTimeout = Just 60000000 }) e
  in EitherT . fmap (first AWSRunError) $ runAWST e' a

-- Unfortunately amazonka doesn't expose this function in the current version
getEnvWithManager :: Region -> AWS.Credentials -> Manager -> IO Env
getEnvWithManager r c m =
  runExceptT (newEnv r c m) >>= either PreludeUnsafe.error return

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

hoistAWSError :: Either AWSError a -> AWS a
hoistAWSError = \case
  Left e -> liftAWSError e
  Right a -> pure a

liftAWSError :: AWSError -> AWS a
liftAWSError = \case
  AWSRegionError e -> fail' regionErrorRender e
  AWSRunError e -> AWS.throwAWSError e

throwError :: (MonadThrow m) => Error -> m a
throwError = \case
  HttpError e -> throwM e
  e@(SerializerError _ _) -> fail' errorRender e
  e@(ServiceError _ _ _) -> fail' errorRender e
  Errors e -> maybe (fail "Error: Unknown") throwError $ listToMaybe e


fail' :: (MonadThrow m) => (e -> Text) -> e -> m a
fail' f =
  throwM . userError . unpack . f

unsafeAWS :: EitherT AWSError IO a -> IO a
unsafeAWS =
  eitherT (fail . show . awsErrorRender) pure
