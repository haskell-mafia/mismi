{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE LambdaCase         #-}
module Mismi.Control (
    module A
  , runAWS
  , runAWSDefaultRegion
  , runAWSWithRegion
  , runAWSWithCreds
  , awsBracket
  , awsBracket_
  , errorRender
  ) where

import           Control.Lens
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

runAWSWithCreds :: Region -> AccessKey -> SecretKey -> Maybe SessionToken -> AWS a -> IO a
runAWSWithCreds r ak sk st a = do
  e <- liftIO . newEnv r $ case st of
    Nothing ->
      FromKeys ak sk
    Just st' ->
      FromSession ak sk st'
  runAWS e a

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

runAWSDefaultRegion :: AWS a -> IO a
runAWSDefaultRegion a = do
  mr <- getRegionFromEnv
  let r = fromMaybe Sydney mr
  e <- newEnv r Discover
  runAWS e a

awsBracket :: AWS a -> (a -> AWS c) -> (a -> AWS b) -> AWS b
awsBracket r f a = do
  e <- ask
  liftIO $ bracket (runAWS e r) (runAWS e . f) (runAWS e . a)

awsBracket_ :: AWS a -> AWS c -> AWS b -> AWS b
awsBracket_ r f a =
  awsBracket r (const f) (const a)

errorRender :: Error -> Text
errorRender = decodeUtf8 . BL.toStrict . toLazyByteString . build
