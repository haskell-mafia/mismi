{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Mismi.S3.Commands where

import           Control.Monad.Catch -- (catchAll)
import           Control.Monad.IO.Class

import qualified Data.ByteString as BS
import           Data.Text (Text)
import           Data.Text.Encoding as T
import qualified Data.Text.IO as T

import           Control.Lens hiding (elements)

import           Mismi.S3
import qualified Mismi.S3.Commands as C

import           Disorder.Core

import           P

import qualified System.Directory as D
import qualified System.FilePath as F
import           System.IO

import           Test.Mismi.S3 (LocalPath (..))
import           Test.Mismi.Amazonka
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


prop_headObject = withAWS $ \a -> do
  h <- C.headObject a
  pure $ h === Nothing

prop_exists = withAWS $ \a -> do
  write a ""
  e <- exists a
  pure $ e === True

prop_exists_failure = withAWS $ \a -> do
  e <- exists a
  pure $ e === False

prop_size t = withAWS $ \a -> do
  write a t
  i <- getSize a
  pure $ i === (Just . BS.length $ T.encodeUtf8 t)

prop_copy t = withAWS' $ \a b -> do
  write a t
  copy a b
  a' <- read a
  b' <- read b
  pure $ a' === b'

prop_copy_overwrite t t' = withAWS' $ \a b -> do
  write a t
  write b t'
  copyWithMode Overwrite a b
  b' <- read b
  pure $ b' === Just t

prop_copy_fail t = withAWS' $ \a b -> do
  write a t
  write b t
  (False <$ copyWithMode Fail a b) `catchAll` (const . pure $ True)

prop_upload d l = withLocalAWS $ \p a -> do
  let t = p F.</> localPath l
  liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
  liftIO $ T.writeFile t d
  upload t a
  r <- read a
  pure $ r === Just d

prop_upload_mode d l m = withLocalAWS $ \p a -> do
  let t = p F.</> localPath l
  liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
  liftIO $ T.writeFile t d
  uploadWithMode m t a
  r <- read a
  pure $ r === Just d

prop_upload_overwrite d1 d2 l = withLocalAWS $ \p a -> do
  let t = p F.</> localPath l
  liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
  liftIO $ T.writeFile t d1
  uploadWithMode Fail t a
  liftIO $ T.writeFile t d2
  uploadWithMode Overwrite t a
  r <- read a
  pure $ r === Just d2

prop_upload_fail d l = withLocalAWS $ \p a -> do
  let t = p F.</> localPath l
  liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
  liftIO $ T.writeFile t d
  uploadWithMode Fail t a
  (False <$ uploadWithMode Fail t a) `catchAll` (const . pure $ True)

prop_download d l = withLocalAWS $ \p a -> do
  let t = p F.</> localPath  l
  write a d
  download a t
  res <- liftIO $ T.readFile t
  pure $ res === d

prop_list_multipart = withMultipart $ \a i -> do
  sendMultipart "" a 1 i
  l <- listMultiparts (bucket a)
  pure $ multipartExists i l

prop_abort_multipart = withMultipart $ \a i -> do
  sendMultipart "" a 1 i
  l <- listMultiparts (bucket a)
  forM_ (findMultiparts i l) $ abortMultipart (bucket a)
  r <- listMultiparts (bucket a)
  pure (neg $ multipartExists i r)

prop_list_parts = withMultipart $ \a i -> do
  sendMultipart "" a 1 i
  l2 <- retryAction $ C.listMultipartParts a i
  pure (length l2 === 1)

prop_list_recursively = withAWS $ \a -> do
  write a ""
  r' <- listRecursively (a { key = dirname $ key a })
  pure $ a `elem` r'

multipartExists :: Text -> [MultipartUpload] -> Property
multipartExists uploadId multiparts =
  count (findMultipart uploadId) multiparts === 1

findMultiparts :: Text -> [MultipartUpload] -> [MultipartUpload]
findMultiparts uploadId =
  filter (findMultipart uploadId)

findMultipart :: Text -> MultipartUpload -> Bool
findMultipart uploadId m =
  m ^. muUploadId == Just uploadId

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
