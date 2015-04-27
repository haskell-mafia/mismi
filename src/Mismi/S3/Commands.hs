{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mismi.S3.Commands (
    exists
  , delete
  , read
  , download
  , write
  , getObjects
  , listRecursively
  ) where

import qualified Aws.S3 as S3

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Catch (catch, throwM)

import           Data.ByteString as BS
import           Data.Conduit
import           Data.Conduit.Binary (sinkFile)
import           Data.Conduit.List as C
import qualified Data.List.NonEmpty as NEL
import           Data.Text as T
import           Data.Text.Encoding as T

import           Mismi.Control
import           Mismi.S3.Control
import           Mismi.S3.Data

import           Network.HTTP.Conduit (responseBody, RequestBody(..))
import           Network.HTTP.Types.Status (status404)

import           P

import           Prelude (error)

import           System.FilePath
import           System.Directory

exists :: Address -> S3Action Bool
exists a =
  let req = S3.headObject (unBucket $ bucket a) (unKey $ key a) in
  awsRequest req >>= pure . isJust . S3.horMetadata

delete :: Address -> S3Action ()
delete a =
  void . awsRequest $ S3.DeleteObject (unKey $ key a) (unBucket $ bucket a)

read :: Address -> S3Action (Maybe Text)
read a =
  let get = S3.getObject (unBucket $ bucket a) (unKey $ key a) in
  (awsRequest get >>=
   fmap Just . lift . fmap (T.decodeUtf8 . BS.concat) . ($$+- C.consume) . responseBody . S3.gorResponse)
  `catch` (\(e :: S3.S3Error) -> if S3.s3StatusCode e == status404 then pure Nothing else throwM e)

download :: Address -> FilePath -> S3Action ()
download a p =
  let get = S3.getObject (unBucket $ bucket a) (unKey $ key a) in do
    whenM (liftIO $ doesFileExist p) . fail $ "Can not download to a target that already exists [" <> p <> "]."
    liftIO $ createDirectoryIfMissing True (dropFileName p)
    awsRequest get >>= lift . ($$+- sinkFile p) . responseBody . S3.gorResponse

write :: WriteMode -> Address -> Text -> S3Action ()
write w a t = do
  case w of
    Fail        -> whenM (exists a) . fail $ "Can not write to a file that already exists [" <> show a <> "]."
    Overwrite   -> return ()
  let body = RequestBodyBS $ T.encodeUtf8 t
  void . awsRequest $ S3.putObject (unBucket $ bucket a) (unKey $ key a) body

getObjects :: Address -> S3Action [S3.ObjectInfo]
getObjects (Address (Bucket b) (Key ky)) =
  getObjects' $ (S3.getBucket b) { S3.gbPrefix = Just $ pp ky }
  where
    pp :: Text -> Text
    pp k = if T.null k then "" else if T.isSuffixOf "/" k then k else k <> "/"
    -- Hoping this will have ok performance in cases where the results are large, it shouldnt
    -- affect correctness since we search through the list for it anyway
    go :: S3.GetBucket -> NEL.NonEmpty S3.ObjectInfo -> S3Action [S3.ObjectInfo]
    go x ks = (NEL.toList ks <>) <$> getObjects' (x { S3.gbMarker = Just $ S3.objectKey $ NEL.last ks })
    getObjects' :: S3.GetBucket -> S3Action [S3.ObjectInfo]
    getObjects' x = do
      resp <- awsRequest x
      if S3.gbrIsTruncated resp
        then
          maybe
            (error "vee: error: truncated response with empty contents list.")
            (go x)
            (NEL.nonEmpty $ S3.gbrContents resp)
        else
          pure $ S3.gbrContents resp

listRecursively :: Address -> S3Action [Address]
listRecursively a =
  fmap (Address (bucket a) . Key . S3.objectKey) <$> getObjects a
