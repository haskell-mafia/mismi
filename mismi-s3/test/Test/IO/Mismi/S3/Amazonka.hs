{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Mismi.S3.Amazonka where

import           Control.Monad.IO.Class

import qualified Data.ByteString as BS
import           Data.Text (Text)
import           Data.Text.Encoding as T
import qualified Data.Text.IO as T

import           Control.Lens hiding (elements)

import           Mismi.S3.Amazonka
import           Mismi.S3.Data
import qualified Mismi.S3.Commands as S3

import           Disorder.Core

import           P

import qualified System.Directory as D
import qualified System.FilePath as F
import           System.IO

import           Test.Mismi.S3 (LocalPath (..))
import           Test.Mismi.Amazonka
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

prop_exists = withAWS $ \a -> do
  liftS3 $ S3.write a ""
  e <- exists a
  pure $ e === True

prop_exists_failure = withAWS $ \a -> do
  e <- exists a
  pure $ e === False

prop_exists_compatibility b = withAWS $ \a -> do
  liftS3 . when b $ S3.write a ""
  e <- exists a
  e' <- liftS3 $ S3.exists a
  pure $ e === e'

prop_size t = withAWS $ \a -> do
  liftS3 $ S3.write a t
  i <- getSize a
  pure $ i === (Just . BS.length $ T.encodeUtf8 t)

prop_copy t = withAWS' $ \a b -> do
  liftS3 $ S3.write a t
  copy a b
  a' <- liftS3 $ S3.read a
  b' <- liftS3 $ S3.read b
  pure $ a' === b'

prop_copy_overwrite t t' = withAWS' $ \a b -> do
  liftS3 $ S3.write a t
  liftS3 $ S3.write b t'
  copyWithMode Overwrite a b
  b' <- liftS3 $ S3.read b
  pure $ b' === Just t

prop_upload d l = withLocalAWS $ \p a -> do
  let t = p F.</> localPath l
  liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
  liftIO $ T.writeFile t d
  upload t a
  r <- liftS3 $ S3.read a
  pure $ r === Just d

prop_download d l = withLocalAWS $ \p a -> do
  let t = p F.</> localPath  l
  liftS3 $ S3.write a d
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
  l2 <- listMultipartParts a i
  pure (length l2 === 1)

prop_list_recursively = withAWS $ \a -> do
  liftS3 $ S3.write a ""
  r' <- listRecursively (a { key = dirname $ key a })
  pure $ a `elem` r'

prop_list_recursively_compatibility = withAWS $ \a -> do
  liftS3 $ S3.write a ""
  r <- listRecursively (a { key = dirname $ key a })
  r' <- liftS3 $ S3.listRecursively (a { key = dirname $ key a })
  pure $ r === r'

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
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 5 })
