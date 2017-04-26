{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Mismi.Control (
    A.AWS
  , A.Error
  , A.AccessKey
  , A.SecretKey
  , A.SessionToken
  , A.Region (..)
  , runAWS
  , runAWST
  , runAWSTWith
  , runAWSTWithRegion
  , rawRunAWS
  , runAWSWithRegion
  , newEnvFromCreds
  , awsBracket
  , awsBracket_
  , renderError
  , onStatus
  , onStatus_
  , handle404
  , handle403
  , handle301
  , setServiceRetry
  , setRetry
  , configureRetries
  , handleServiceError
  , withRetries
  , withRetriesOf
  ) where

import           Control.Exception (IOException)
import           Control.Lens ((.~), (^.), (^?), over)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Retry (RetryPolicyM, fullJitterBackoff, recovering, rsIterNumber)

import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Builder
import           Data.Text as T
import           Data.Text.Encoding as T

import           Mismi.Environment

import           Network.AWS hiding (runAWS)
import qualified Network.AWS as A
import           Network.AWS.Data
import           Network.AWS.Error

import           Network.HTTP.Client (HttpException (..))
import           Network.HTTP.Client.Internal (mResponseTimeout)
import           Network.HTTP.Types.Status

import           P

import           System.IO

import           X.Control.Monad.Trans.Either

runAWST :: Env -> (Error -> e) -> EitherT e AWS a -> EitherT e IO a
runAWST e err action =
  runAWSTWith (runAWS e) err action

runAWSTWithRegion :: Region -> (Error -> e) -> EitherT e AWS a -> EitherT e IO a
runAWSTWithRegion r err action =
  runAWSTWith (runAWSWithRegion r) err action

runAWSTWith :: (forall b. AWS b -> EitherT Error IO b) -> (Error -> e) -> EitherT e AWS a -> EitherT e IO a
runAWSTWith run err action =
  joinErrors id err $ mapEitherT run action

runAWS :: (MonadIO m, MonadCatch m) => Env -> AWS a -> EitherT Error m a
runAWS e'' =
  let e' = over envManager (\m -> m { mResponseTimeout = Just 60000000 }) e''
      e = configureRetries 5 e'
  in EitherT . try . liftIO . rawRunAWS e

runAWSWithRegion :: (MonadIO m, MonadCatch m) => Region -> AWS a -> EitherT Error m a
runAWSWithRegion r a = do
  e <- liftIO $ discoverAWSEnvWithRegion r
  runAWS e a

newEnvFromCreds :: (Applicative m, MonadIO m, MonadCatch m) => Region -> AccessKey -> SecretKey -> Maybe SessionToken -> m Env
newEnvFromCreds r ak sk st =
  newEnv r $ case st of
    Nothing ->
      FromKeys ak sk
    Just st' ->
      FromSession ak sk st'

rawRunAWS :: Env -> AWS a -> IO a
rawRunAWS e =
  runResourceT . A.runAWS e

awsBracket :: AWS a -> (a -> AWS c) -> (a -> AWS b) -> AWS b
awsBracket r f a = do
  e <- ask
  liftIO $ bracket (unsafeRunAWS e r) (unsafeRunAWS e . f) (unsafeRunAWS e . a)

awsBracket_ :: AWS a -> AWS c -> AWS b -> AWS b
awsBracket_ r f a =
  awsBracket r (const f) (const a)

unsafeRunAWS :: Env -> AWS a -> IO a
unsafeRunAWS e a =
  eitherT throwM pure $ runAWS e a

renderError :: Error -> Text
renderError =
  decodeUtf8 . BL.toStrict . toLazyByteString . build

setServiceRetry :: Retry -> AWS a -> AWS a
setServiceRetry r =
  local (override (serviceRetry .~ r))

setRetry :: Int -> AWS a -> AWS a
setRetry =
  local . configureRetries

withRetries :: (MonadCatch m, MonadMask m, MonadIO m) => Int -> m a -> m a
withRetries =
  withRetriesOf (fullJitterBackoff 500000)

withRetriesOf :: (MonadCatch m, MonadMask m, MonadIO m) => RetryPolicyM m -> Int -> m a -> m a
withRetriesOf policy n action = do
  let
    httpCondition s =
      Handler $ \(e :: HttpException) ->
        pure $
          if rsIterNumber s > n
            then False
            else checkException e False

    ioCondition s =
      Handler $ \(_ :: IOException) ->
        pure $ rsIterNumber s < n

  recovering policy [httpCondition, ioCondition] $ \_ ->
    action

configureRetries :: Int -> Env -> Env
configureRetries i e = e & envRetryCheck .~ err
  where
    err c _ | c >= i = False
    err c v =
      checkException v $ (e ^. envRetryCheck) c v

checkException :: HttpException -> Bool -> Bool
checkException v f =
  case v of
    NoResponseDataReceived ->
      True
    StatusCodeException status _ _ ->
      status == status500 || status == status503
    FailedConnectionException _ _ ->
      True
    FailedConnectionException2 _ _ _ _ ->
      True
    TlsException _ ->
      True
    InternalIOException _ ->
      True
    HandshakeFailed ->
      True
    ResponseTimeout ->
      True
    ResponseBodyTooShort _ _ ->
      True
#if MIN_VERSION_http_client(0, 4, 24)
    TlsExceptionHostPort _ _ _ ->
      True
#endif
    _ ->
      f

handle404 :: AWS a -> AWS (Maybe a)
handle404 =
  handleStatus status404

handle403 :: AWS a -> AWS (Maybe a)
handle403 =
  handleStatus status403

handle301 :: AWS a -> AWS (Maybe a)
handle301 =
  handleStatus status301

handleStatus :: Status -> AWS a -> AWS (Maybe a)
handleStatus s m =
  fmap Just m `catch` \(e :: Error) ->
    if e ^? httpStatus == Just s then return Nothing else throwM e

-- | return a result code depending on the HTTP status
onStatus :: (Status -> Maybe r) -> AWS a -> AWS (Either r a)
onStatus f m =
  fmap Right m `catch` \(e :: Error) ->
    case e ^? httpStatus >>= f of
      Just r1 ->
        return (Left r1)
      Nothing ->
        throwM e

-- | return a result code depending on the HTTP status
--   for an AWS action returning no value
onStatus_ :: r -> (Status -> Maybe r) -> AWS () -> AWS r
onStatus_ r f m =
  fmap (const r) m `catch` \(e :: Error) ->
    case e ^? httpStatus >>= f of
      Just r1 ->
        return r1
      Nothing ->
        throwM e

handleServiceError :: (ServiceError -> Bool) -> (ServiceError -> a) -> AWS a -> AWS a
handleServiceError f pass action =
  action `catch` \(e :: Error) ->
    case e of
      ServiceError se ->
        if f se
          then pure $ pass se
          else throwM e
      SerializeError _ ->
        throwM e
      TransportError _ ->
        throwM e
