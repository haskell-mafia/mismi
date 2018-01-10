{-# LANGUAGE CPP #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  , throwOrRetry
  , throwOrRetryOf
  ) where

import           Control.Exception (IOException)
import           Control.Lens ((.~), (^.), (^?), over)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Retry (RetryPolicyM, RetryStatus)
import           Control.Retry (fullJitterBackoff, recovering, rsIterNumber, applyPolicy)

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
#if MIN_VERSION_http_client(0,5,0)
import           Network.HTTP.Client (HttpExceptionContent (..), responseTimeoutMicro, responseStatus)
#endif
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
  let
    e' = over envManager (\m -> m { mResponseTimeout =
#if MIN_VERSION_http_client(0,5,0)
        responseTimeoutMicro 60000000
#else
        Just 60000000
#endif
      }) e''
    e = configureRetries 5 e'
  in
    EitherT . try . liftIO . rawRunAWS e

runAWSWithRegion :: (MonadIO m, MonadCatch m) => Region -> AWS a -> EitherT Error m a
runAWSWithRegion r a = do
  e <- liftIO $ discoverAWSEnvWithRegion r
  runAWS e a

newEnvFromCreds :: (Applicative m, MonadIO m, MonadCatch m) => Region -> AccessKey -> SecretKey -> Maybe SessionToken -> m Env
newEnvFromCreds r ak sk st = do
#if MIN_VERSION_amazonka(1,4,4)
  e <- newEnv $ case st of
    Nothing ->
      FromKeys ak sk
    Just st' ->
      FromSession ak sk st'
  pure $ e & envRegion .~ r
#else
  newEnv r $ case st of
    Nothing ->
      FromKeys ak sk
    Just st' ->
      FromSession ak sk st'
#endif

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
  recovering policy [httpCondition n, ioCondition n] $ \_ ->
    action

httpCondition :: Applicative m => Int -> RetryStatus -> Handler m Bool
httpCondition n s =
  Handler $ \(e :: HttpException) ->
    pure $
      if rsIterNumber s > n
        then False
        else checkException e False

ioCondition :: Applicative m => Int -> RetryStatus -> Handler m Bool
ioCondition n s =
  Handler $ \(_ :: IOException) ->
    pure $ rsIterNumber s < n

throwOrRetry ::
     (MonadCatch m, MonadMask m, MonadIO m)
   => Int
   -> SomeException
   -> RetryStatus
   -> m RetryStatus
throwOrRetry =
  throwOrRetryOf (fullJitterBackoff 500000)

throwOrRetryOf ::
     (MonadCatch m, MonadMask m, MonadIO m)
   => RetryPolicyM m
   -> Int
   -> SomeException
   -> RetryStatus
   -> m RetryStatus
throwOrRetryOf policy n ex0 s0 =
  let
    recover = \case
      [] ->
        throwM ex0

      h0 : hs ->
        case h0 s0 of
          Handler h ->
            case fromException ex0 of
              Nothing ->
                recover hs

              Just ex -> do
                ok <- h ex
                if ok then do
                  ms <- applyPolicy policy s0
                  case ms of
                    Nothing ->
                      throwM ex

                    Just s ->
                      pure s

                else
                  throwM ex
  in
    recover [httpCondition n, ioCondition n]

configureRetries :: Int -> Env -> Env
configureRetries i e = e & envRetryCheck .~ err
  where
    err c _ | c >= i = False
    err c v =
      checkException v $ (e ^. envRetryCheck) c v

checkException :: HttpException -> Bool -> Bool
checkException v f =
  case v of
#if MIN_VERSION_http_client(0,5,0)
    InvalidUrlException _ _ ->
      False
    HttpExceptionRequest _req content ->
      case content of
        NoResponseDataReceived ->
          True
        StatusCodeException resp _ ->
          let status = responseStatus resp in
          status == status500 || status == status503
        ResponseTimeout ->
          True
        ConnectionTimeout ->
          True
        ConnectionFailure _ ->
          True
        ResponseBodyTooShort _ _ ->
          True
        InternalException _ ->
          True
        InvalidStatusLine _ ->
          True
        InvalidHeader _ ->
          True
        ProxyConnectException _ _ _ ->
          True
        WrongRequestBodyStreamSize _ _ ->
          True
        InvalidChunkHeaders ->
          True
        IncompleteHeaders ->
          True
        HttpZlibException _ ->
          True

        TooManyRedirects _ ->
          False
        OverlongHeaders ->
          False
        TlsNotSupported ->
          False
        InvalidDestinationHost _ ->
          False
        InvalidProxyEnvironmentVariable _ _ ->
          False

        _ ->
          f
#else
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
    _ ->
      f
#endif

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

-- | Return a result code depending on the HTTP status
onStatus :: (Status -> Maybe r) -> AWS a -> AWS (Either r a)
onStatus f m =
  fmap Right m `catch` \(e :: Error) ->
    case e ^? httpStatus >>= f of
      Just r1 ->
        return (Left r1)
      Nothing ->
        throwM e

-- | Return a result code depending on the HTTP status
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
