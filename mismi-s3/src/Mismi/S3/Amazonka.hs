{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}
module Mismi.S3.Amazonka (
    module AWS
  , module A
  , headObject
  , exists
  , getSize
  , delete
  , copy
  , copyWithMode
  , upload
  , download
  , downloadWithRange
  , listMultipartParts
  , listMultiparts
  , listOldMultiparts
  , listOldMultiparts'
  , abortMultipart
  , abortMultipart'
  , filterOld
  , filterNDays
  , listRecursively
  , listRecursively'
  , sync
  , syncWithMode
  , retryAWSAction
  , retryAWSAction'
  , retryAWS
  , retryAWS'
  , sse
  , retryConduit
  ) where


import           Control.Concurrent

import           Control.Lens
import           Control.Retry
import           Control.Monad.Catch
-- import           Control.Monad.Morph (hoist)
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Either hiding (hoistEither)
import           Control.Monad.Trans.Resource
import           Control.Monad.Reader (ask, local)
import           Control.Monad.IO.Class

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import           Data.Conduit
import qualified Data.Conduit.List as DC
import           Data.Conduit.Binary

import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock

import           Mismi.Control.Amazonka hiding (AWSError, throwAWSError)
import qualified Mismi.Control.Amazonka as A
import           Mismi.S3.Data
import           Mismi.S3.Internal

import           Network.AWS.S3 hiding (headObject, Bucket, bucket)
import qualified Network.AWS.S3 as AWS
import           Network.AWS.Data
import           Network.HTTP.Types.Status (status500, status404)

import           P

import           System.IO
import           System.IO.Error (userError)
import           System.Directory
import           System.FilePath hiding ((</>))
import           System.Posix.IO
import qualified "unix-bytestring" System.Posix.IO.ByteString as UBS

-- HACKS
import           Network.HTTP.Conduit

headObject :: Address -> AWS (Maybe AWS.HeadObjectResponse)
headObject a = do
  res <- sendCatch $ fencode' AWS.headObject a
  handle404 res

exists :: Address -> AWS Bool
exists a =
  headObject a >>= pure . maybe False (const True)

getSize :: Address -> AWST IO (Maybe Int)
getSize a =
  headObject a >>= pure . maybe Nothing (^. horContentLength)

delete :: Address -> AWS ()
delete =
  send_ . fencode' deleteObject

handle404 :: Either (ServiceError RESTError) a -> AWS (Maybe a)
handle404 res = case res of
  Left e -> case e of
    HttpError e' -> case e' of
      StatusCodeException s _ _ ->
        if s == status404
          then pure Nothing
          else throwAWSError e
      _ -> throwAWSError e
    SerializerError _ _ -> throwAWSError e
    ServiceError _ _ _ -> throwAWSError e
    Errors _ -> throwAWSError e
  Right rs -> pure $ Just rs

copy :: Address -> Address -> AWS ()
copy source dest =
  copyWithMode Fail source dest

copyWithMode :: WriteMode -> Address -> Address -> AWS ()
copyWithMode mode s d = do
  unlessM (exists s) . fail $ "Can not copy when the soruce does not exists exists [" <> (T.unpack $ addressToText s)  <> "]."
  foldWriteMode  (whenM (exists d) . fail $ "Can not copy to a file that already exists [" <> (T.unpack $ addressToText d) <> "].") (pure ()) mode
  copy' s d

copy' :: Address -> Address -> AWS ()
copy' (Address (Bucket sb) (Key sk)) (Address (Bucket b) k) =
  send_ $ copyObject b (sb <> "/" <> sk) (encodeKey k) & coServerSideEncryption .~ Just sse & coMetadataDirective .~ Just Copy

upload :: FilePath -> Address -> AWS ()
upload f a = do
   x <- liftIO $ LBS.readFile f
   send_ $ fencode' (putObject $ toBody x) a & poServerSideEncryption .~ Just sse

download :: Address -> FilePath -> AWS ()
download a f = do
  liftIO $ createDirectoryIfMissing True (dropFileName f)
  r <- send $ fencode' getObject a
  liftIO . withFileSafe f $ \f'' ->
    runResourceT . ($$+- sinkFile f'') $ r ^. gorBody ^. _RsBody

downloadWithRange :: Address -> Int -> Int -> FilePath -> AWS ()
downloadWithRange source start end dest = do
  let req = fencode' getObject source & goRange .~ (Just $ downRange start end)
  r <- send req
  let p :: AWS.GetObjectResponse = r
  let y :: RsBody = p ^. AWS.gorBody

  fd <- liftIO $ openFd dest WriteOnly Nothing defaultFileFlags
  void . liftIO $ fdSeek fd AbsoluteSeek (fromInteger . toInteger $ start)
  liftIO $ do
    let rs :: ResumableSource (ResourceT IO) BS.ByteString = y ^. _RsBody
    let s = awaitForever $ \bs -> liftIO $
              UBS.fdWrite fd bs
    runResourceT $ ($$+- s) rs
  liftIO $ closeFd fd

listMultipartParts :: Address -> T.Text -> AWS [Part]
listMultipartParts a uploadId = do
  let req = fencode' AWS.listParts a uploadId
  paginate req $$ DC.foldMap (^. lprParts)

listMultiparts :: Bucket -> AWS [MultipartUpload]
listMultiparts b = do
  let req = listMultipartUploads $ unBucket b
  paginate req $$ DC.foldMap (^. lmurUploads)

listOldMultiparts :: Bucket -> AWS [MultipartUpload]
listOldMultiparts b = do
  mus <- listMultiparts b
  now <- liftIO getCurrentTime
  pure $ filter (filterOld now) mus

listOldMultiparts' :: Bucket -> Int -> AWS [MultipartUpload]
listOldMultiparts' b i = do
  mus <- listMultiparts b
  now <- liftIO getCurrentTime
  pure $ filter (filterNDays i now) mus

filterOld :: UTCTime -> MultipartUpload -> Bool
filterOld = filterNDays 7

filterNDays :: Int -> UTCTime -> MultipartUpload -> Bool
filterNDays n now m = case m ^. muInitiated of
  Nothing -> False
  Just x -> nDaysOld n now x

nDaysOld :: Int -> UTCTime -> UTCTime -> Bool
nDaysOld n now utc = do
  let n' = fromInteger $ toInteger n
  let diff = -1 * 60 * 60 * 24 * n' :: NominalDiffTime
  let boundary = addUTCTime diff now
  boundary > utc

abortMultipart :: Bucket -> MultipartUpload -> AWST IO ()
abortMultipart (Bucket b) mu = do
  let x :: String -> ServiceError String = ServiceError "amu" status500
  k <- maybe (throwAWSError $ x "Multipart key missing") pure (mu ^. muKey)
  i <- maybe (throwAWSError $ x "Multipart uploadId missing") pure (mu ^. muUploadId)
  abortMultipart' (Address (Bucket b) (Key k)) i

abortMultipart' :: Address -> T.Text -> AWS ()
abortMultipart' a i =
  send_ $ fencode' abortMultipartUpload a i

listRecursively :: Address -> AWS [Address]
listRecursively a = do
  a' <- listRecursively' a
  retryAWSAction $ a' $$ DC.consume

listRecursively' :: Address -> AWS (Source AWS Address)
listRecursively' a@(Address (Bucket b) (Key k)) =
  pure $ (paginate $ listObjects b & loPrefix .~ Just k) =$= liftAddress a

liftAddress :: Address -> Conduit ListObjectsResponse AWS Address
liftAddress a =
  DC.mapFoldable (\r -> (\o -> a { key = Key $ o ^. oKey }) <$> (r ^. lorContents) )

retryConduit :: Env -> AWS a -> AWS a
retryConduit e action =
  local (const e) action

sync :: Address -> Address -> Int -> AWS ()
sync =
  syncWithMode FailSync

syncWithMode :: SyncMode -> Address -> Address -> Int -> AWS ()
syncWithMode mode source dest fork = do
  (c, r) <- liftIO $ (,) <$> newChan <*> newChan
  e <- ask
  m <- liftIO $ newManager $ conduitManagerSettings { managerConnCount = fork }
  let !e' = set envManager m e

  -- worker
  tid <- liftIO $ forM [1..fork] (\i -> forkIO $ worker i source dest mode e' c r)

  -- sink list to channel
  l <- listRecursively' source
  i <- sinkChanWithDelay 50000 l c

  -- wait for threads and lift errors
  r' <- liftIO $ waitForNResults i r
  liftIO $ forM_ tid killThread
  forM_ r' hoistWorkerResult

hoistWorkerResult :: WorkerResult -> AWS ()
hoistWorkerResult =
  foldWR hoistErr (pure ())

hoistErr :: Err -> AWS ()
hoistErr =
  foldErr throwM (throwM . userError . T.unpack) liftAWSError

worker :: Int -> Address -> Address -> SyncMode -> Env -> Chan Address -> Chan WorkerResult -> IO ()
worker _ source dest mode e c errs = do
--  m <- newManager $ conduitManagerSettings { managerConnCount = 1 }
--  let !e'' = set envManager m e
 let e'' = e
 forever $ do
  let invariant = pure . WorkerErr $ Invariant "removeCommonPrefix"
      keep :: Address -> Key -> IO WorkerResult
      keep a k = do
        e' <- runEitherT $ keep' a k
        pure $ case e' of
          Left er -> WorkerErr er
          Right _ -> WorkerOk

      keep' :: Address -> Key -> EitherT Err IO ()
      keep' a k = do
        let out = withKey (</> k) dest
            action :: EitherT Err AWS ()
            action = EitherT $ do
              let cp = copy a out >>= pure . Right
                  ex = exists out
                  te = pure . Left $ Target a out
              foldSyncMode
                (ifM ex te cp)
                cp
                (ifM ex (pure $ Right ()) cp)
                mode
        (mapEitherT (runEitherT . bimapEitherT AwsErr id . runAWSWithEnv e'') action >>= EitherT . pure)
          `catchAll` (left . UnknownErr)

  a <- readChan c
  wr <- maybe invariant (keep a) $ removeCommonPrefix source a
  writeChan errs wr

data WorkerResult =
  WorkerOk
  | WorkerErr Err

foldWR :: (Err -> m a) -> m a -> WorkerResult -> m a
foldWR e a = \case
  WorkerOk -> a
  WorkerErr err -> e err

data Err =
  Invariant Text
  | Target Address Address
  | AwsErr A.AWSError
  | UnknownErr SomeException

foldErr :: (SomeException -> m a) -> (Text -> m a) -> (A.AWSError -> m a) -> Err -> m a
foldErr se p err = \case
  Invariant t -> p $ "[Mismi internal error] - " <> t
  Target a o -> p $ "[Mismi internal error] - " <> "Can not copy [" <> addressToText a <> "] to [" <> addressToText o <> "]. Target file exists"
  AwsErr e -> err e
  UnknownErr e -> se e

retryAWSAction :: AWS a -> AWS a
retryAWSAction =
  retryAWSAction' (retryWithBackoff 5)

retryAWSAction' :: RetryPolicy -> AWS a -> AWS a
retryAWSAction' rp a = do
  local (retryAWS' rp) $ a

retryAWS :: Int -> Env -> Env
retryAWS i = retryAWS' (retryWithBackoff i)

retryAWS' :: RetryPolicy -> Env -> Env
retryAWS' r e =
  let err c v = case v of
        NoResponseDataReceived -> pure True
        StatusCodeException s _ _ -> pure $ s == status500
        FailedConnectionException _ _ -> pure True
        FailedConnectionException2 _ _ _ _ -> pure True
        TlsException _ -> pure True
        _ -> (e ^. envRetryCheck) c v
  in
  e & envRetryPolicy .~ Just r & envRetryCheck .~ err

sse :: ServerSideEncryption
sse =
  AES256
