{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Mismi.S3.Data (
    PartResponse (..)
  , WriteMode (..)
  , SyncMode (..)
  , Bucket (..)
  , Address (..)
  , Key (..)
  , ReadGrant (..)
  , Upload (..)
  , S3Error (..)
  , ErrorType (..)
  , DownloadError (..)
  , ConcatError (..)
  , CopyError (..)
  , UploadError (..)
  , SyncError (..)
  , SyncWorkerError (..)
  , WriteResult (..)
  , (//)
  , combineKey
  , dirname
  , foldWriteMode
  , foldSyncMode
  , basename
  , addressFromText
  , addressToText
  , removeCommonPrefix
  , withKey
  , s3Parser
  , s3ErrorRender
  , renderDownloadError
  , renderConcatError
  , renderCopyError
  , renderUploadError
  , renderSyncError
  , renderSyncWorkerError
  , sse
  ) where

import           Control.Exception (Exception)

import qualified Data.Text as T
import           Data.Typeable (Typeable)

import           P

import           Mismi (Error, renderError)
                 -- Just for compatibility, would be good to not do
                 -- this at some point but for now we import everything
                 -- and keep current export list.
import           Mismi.S3.Core.Data (Address (..), Bucket (..), Key (..), ReadGrant (..)
                                    , SyncMode (..), WriteMode (..), WriteResult (..), (//)
                                    , addressFromText, addressToText, basename, combineKey
                                    , dirname, foldSyncMode, foldWriteMode, removeCommonPrefix
                                    , s3Parser, withKey)
import           Network.AWS.S3 (ETag, ServerSideEncryption (..))

import           System.FilePath (FilePath)

import           Twine.Parallel (RunError (..), renderRunError)


data PartResponse =
    PartResponse !Int !ETag
    deriving (Eq, Show)

data S3Error =
    SourceMissing ErrorType Address
  | SourceFileMissing FilePath
  | DestinationAlreadyExists Address
  | DestinationDoesNotExist Address
  | DestinationFileExists FilePath
  | DestinationNotDirectory FilePath
  | DestinationMissing FilePath
  | SourceNotDirectory FilePath
  | AccessDenied Address
  | Invariant Text
  | Target Address Address
  | MissingETag
  deriving (Eq, Typeable)

instance Exception S3Error

instance Show S3Error where
  show = T.unpack . s3ErrorRender

s3ErrorRender :: S3Error -> Text
s3ErrorRender s3err = "[Mismi internal error] - " <> case s3err of
  SourceMissing e a ->
    "Can not " <> renderErrorType e <> " when the source object does not exist [" <> addressToText a <> "]"
  SourceFileMissing f ->
    "Can not upload when the source file does not exist [" <> T.pack f <> "]"
  DestinationAlreadyExists a ->
    "Can not upload to an address that already exists [" <> addressToText a <> "]"
  DestinationFileExists f ->
    "Can not download to a target that already exists [" <> T.pack f <> "]"
  DestinationNotDirectory f ->
    "Expecting destination " <> T.pack f <> " to be a directory."
  DestinationMissing f ->
    "Download destination directory " <> T.pack f <> " does not exist."
  SourceNotDirectory f ->
    "Recursive upload source " <> T.pack f <> " must be a directory."
  DestinationDoesNotExist a ->
    "This address does not exist [" <> addressToText a <> "]"
  AccessDenied a ->
    "The access to this address is denied [" <> addressToText a <> "]"
  Invariant e ->
    e
  Target a o ->
    "Can not copy [" <> addressToText a <> "] to [" <> addressToText o <> "]. Target file exists"
  MissingETag ->
    "missing ETag"

data ErrorType =
    DownloadError
  | CopyError'
  deriving (Eq, Show)

renderErrorType :: ErrorType -> Text
renderErrorType e = case e of
  DownloadError ->
    "download"
  CopyError' ->
    "copy"

data DownloadError =
    DownloadSourceMissing Address
  | DownloadDestinationExists FilePath
  | DownloadDestinationNotDirectory FilePath
  | DownloadInvariant Address Address
  | MultipartError (RunError Error)
  deriving Show

renderDownloadError :: DownloadError -> Text
renderDownloadError d =
  case d of
    DownloadSourceMissing a ->
      "Can not download when the source object does not exist [" <> addressToText a <> "]"
    DownloadDestinationExists f ->
      "Can not download to a target that already exists [" <> T.pack f <> "]"
    DownloadDestinationNotDirectory f ->
      "Destination for a recursive download, " <> T.pack f <> " is not a directory."
    DownloadInvariant a b ->
      "Remove common prefix invariant: " <>
      "[" <> addressToText b <> "] is not a common prefix of " <>
      "[" <> addressToText a <> "]"
    MultipartError r ->
      "Multipart download error: " <> renderRunError r renderError

data ConcatError =
    ConcatSourceMissing Address
  | ConcatDestinationExists Address
  | ConcatCopyError (RunError Error)
  | NoInputFiles
  | NoInputFilesWithData
  | ConcatSourceTooSmall Address Int
    deriving Show

renderConcatError :: ConcatError -> Text
renderConcatError e =
  case e of
    ConcatSourceMissing a ->
      "Can not concat objects when the source object does not exist [" <> addressToText a <> "]"
    ConcatDestinationExists a ->
      "Can not concat objects when the destination object already exists [" <> addressToText a <> "]"
    ConcatCopyError a ->
      renderRunError a ((<>) "Multipart concat failed on a worker: " . renderError)
    NoInputFiles ->
      "Can not concat with no input keys."
    NoInputFilesWithData ->
      "Can not concat with no input keys with data."
    ConcatSourceTooSmall a s ->
      T.intercalate " " [
          "Source file"
        , addressToText a
        , "(" <> renderIntegral s <> ") bytes"
        , "is too small to use as part of a multipart upload."
        ]

data CopyError =
    CopySourceMissing Address
  | CopyDestinationExists Address
  | CopySourceSize Address
  | MultipartCopyError (RunError Error)

renderCopyError :: CopyError -> Text
renderCopyError e =
  case e of
    CopySourceMissing a ->
      "Can not copy an object when the source object does not exist [" <> addressToText a <> "]"
    CopyDestinationExists a ->
      "Can not copy an object when the destination object already exists [" <> addressToText a <> "]"
    CopySourceSize a ->
      "Can not calculate the size of the source object [" <> addressToText a <> "]"
    MultipartCopyError a ->
      renderRunError a ((<>) "Multipart copy failed on a worker: " . renderError)

data UploadError =
    UploadSourceMissing FilePath
  | UploadDestinationExists Address
  | UploadSourceNotDirectory FilePath
  | MultipartUploadError (RunError Error)
  deriving Show

renderUploadError :: UploadError -> Text
renderUploadError e =
  case e of
    UploadSourceMissing f ->
      "Can not upload when the source file does not exist [" <> T.pack f <> "]"
    UploadDestinationExists a ->
      "Can not upload when the destination object already exists [" <> addressToText a <> "]"
    UploadSourceNotDirectory f ->
      "Recursive upload source " <> T.pack f <> " must be a directory."
    MultipartUploadError a ->
      renderRunError a ((<>) "Multipart upload failed on a worker: " . renderError)

newtype SyncError =
  SyncError (RunError SyncWorkerError)

renderSyncError :: SyncError -> Text
renderSyncError (SyncError r) =
  renderRunError r renderSyncWorkerError

data SyncWorkerError =
   SyncInvariant Address Address
 | OutputExists Address
 | SyncAws Error
 | SyncCopyError CopyError

renderSyncWorkerError :: SyncWorkerError -> Text
renderSyncWorkerError w =
  case w of
    SyncInvariant a b ->
      "Remove common prefix invariant: " <>
      "[" <> addressToText b <> "] is not a common prefix of " <>
      "[" <> addressToText a <> "]"
    OutputExists a ->
      "Can not copy to an address that already exists [" <> addressToText a <> "]"
    SyncAws e ->
      "AWS failure during 'sync': " <> renderError e
    SyncCopyError c ->
      "Copy failure during 'sync': " <> renderCopyError c


data Upload =
    UploadSingle
  | UploadMultipart Integer Integer
  deriving (Eq, Show)

sse :: ServerSideEncryption
sse =
  AES256
