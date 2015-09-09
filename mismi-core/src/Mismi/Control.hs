{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mismi.Control (
    module A
  , runAWS
  , runAWST
  , runAWSDefaultRegion
  , runAWSDefaultRegionT
  , runAWSWithRegion
  , runAWSWithRegionT
  , runAWSWithCreds
  , runAWSWithCredsT
  , awsBracket
  , awsBracket_
  , errorRender
  ) where

import           Control.Lens
import           Control.Monad.Trans.Either
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Builder
import           Data.Text as T
import           Data.Text.Encoding as T

import           Mismi.Environment

import           Network.AWS hiding (runAWS)
import qualified Network.AWS as A
import           Network.AWS.Data as A
import           Network.AWS.Error as A
import           Network.AWS.Waiter as A

import           Network.HTTP.Client.Internal (mResponseTimeout)

import           P

import           System.IO
import           System.Environment


runAWS :: Env -> AWS a -> IO a
runAWS e =
  let e' = over envManager (\m -> m { mResponseTimeout = Just 60000000 }) e
  in runResourceT . A.runAWS e'

runAWST :: (MonadIO m, MonadCatch m) => Env -> AWS a -> EitherT Error m a
runAWST e =
  catchError . runAWS e

catchError :: (MonadIO m, MonadCatch m) => IO a -> EitherT Error m a
catchError =
  EitherT . try . liftIO

runAWSWithCreds :: Region -> AccessKey -> SecretKey -> Maybe SessionToken -> AWS a -> IO a
runAWSWithCreds r ak sk st a = do
  e <- liftIO . newEnv r $ case st of
    Nothing ->
      FromKeys ak sk
    Just st' ->
      FromSession ak sk st'
  runAWS e a

runAWSWithCredsT :: (MonadIO m, MonadCatch m) => Region -> AccessKey -> SecretKey -> Maybe SessionToken -> AWS a -> EitherT Error m a
runAWSWithCredsT r ak sk st =
  catchError . runAWSWithCreds r ak sk st

runAWSWithRegion :: Region -> AWS a -> IO a
runAWSWithRegion r a = do
  e <- newEnv r Discover
  token' <- lookupEnv "AWS_SECURITY_TOKEN"
  e' <- maybe
           (pure e)
           (\token -> do
               auth <- withAuth (e ^. envAuth) (\ae -> pure . Auth $ ae { _authToken = Just token })
               pure $ e & envAuth .~ auth)
           (SessionToken . BS.pack <$> token')
  runAWS e' a

runAWSWithRegionT :: (MonadIO m, MonadCatch m) => Region -> AWS a -> EitherT Error m a
runAWSWithRegionT r =
  catchError . runAWSWithRegion r

runAWSDefaultRegion :: AWS a -> IO a
runAWSDefaultRegion a = do
  mr <- getRegionFromEnv
  let r = fromMaybe Sydney mr
  e <- newEnv r Discover
  runAWS e a

runAWSDefaultRegionT :: (MonadIO m, MonadCatch m) => AWS a -> EitherT Error m a
runAWSDefaultRegionT =
  catchError . runAWSDefaultRegion

awsBracket :: AWS a -> (a -> AWS c) -> (a -> AWS b) -> AWS b
awsBracket r f a = do
  e <- ask
  liftIO $ bracket (runAWS e r) (runAWS e . f) (runAWS e . a)

awsBracket_ :: AWS a -> AWS c -> AWS b -> AWS b
awsBracket_ r f a =
  awsBracket r (const f) (const a)

errorRender :: Error -> Text
errorRender = decodeUtf8 . BL.toStrict . toLazyByteString . build
