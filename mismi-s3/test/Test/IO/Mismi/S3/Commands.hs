{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Mismi.S3.Commands where

import           Control.Concurrent
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Lens hiding (elements)

import "cryptohash" Crypto.Hash

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import           Data.Maybe
import           Data.Text as T hiding (copy, length)
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import           Disorder.Core
import           Disorder.Corpus

import           Mismi.S3
import qualified Mismi.S3.Amazonka as A

import           P

import qualified System.Directory as D
import qualified System.FilePath as F
import           System.IO

import           Test.Mismi.Amazonka
import           Test.Mismi.S3
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


prop_exists = withAWS $ \a -> do
  writeOrFail a ""
  e <- exists a
  pure $ e === True

prop_exists_empty = withAWS $ \a ->
  not <$> exists a

prop_exists_failure = withAWS $ \a -> do
  e <- exists a
  pure $ e === False

prop_headObject = withAWS $ \a -> do
  h <- headObject a
  pure $ h === Nothing


prop_getObjects_empty = withAWS $ \a -> do
  objs <- getObjectsRecursively a
  pure $ objs === []

prop_getObjectsR :: Text -> Key -> Key -> Property
prop_getObjectsR t p1 p2 = p1 /= p2 ==> withAWS $ \root -> do
  let keys = [p1, p2 </> p1, p2 </> p2]
  forM_ keys $ \k -> writeOrFail (withKey (</> k) root) t
  objs <- getObjectsRecursively root
  pure $ on (===) L.sort ((^. A.oKey . to A.toText) <$> objs) (unKey . (</>) (key root) <$> keys)

prop_getObjs = forAll ((,) <$> elements muppets <*> choose (1000, 1500)) $ \(m, n) -> withAWS $ \a -> do
  forM_ [1..n] $ \n' -> writeOrFail (withKey(</> Key (m <> pack (show n'))) a) ""
  r' <- list a
  pure $ length r' === n

prop_size :: Text -> Property
prop_size t = withAWS $ \a -> do
  writeOrFail a t
  i <- getSize a
  pure $ i === (Just . BS.length $ T.encodeUtf8 t)

prop_size_failure = withAWS $ \a -> do
  i <- getSize a
  pure $ i === Nothing

prop_copy :: Text -> Property
prop_copy t = withAWS' $ \a b -> do
  writeOrFail a t
  copy a b
  a' <- read a
  b' <- read b
  pure $ a' === b'

prop_copy_overwrite :: Text -> Text -> Property
prop_copy_overwrite t t' = withAWS' $ \a b -> do
  writeOrFail a t
  writeOrFail b t'
  copyWithMode Overwrite a b
  b' <- read b
  pure $ b' === Just t

prop_copy_fail :: Text -> Property
prop_copy_fail t = withAWS' $ \a b -> do
  writeOrFail a t
  writeOrFail b t
  (False <$ copyWithMode Fail a b) `catchAll` (const . pure $ True)

prop_move :: Text -> Token -> Property
prop_move t d' = withAWS $ \s ->
  withToken d' $ \d -> do
    writeOrFail s t
    move s d
    es <- exists s
    ed <- exists d
    pure $ (es, ed) === (False, True)

prop_upload_mode :: Text -> LocalPath -> WriteMode -> Property
prop_upload_mode d l m = withLocalAWS $ \p a -> do
  let t = p F.</> localPath l
  liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
  liftIO $ T.writeFile t d
  uploadWithModeOrFail m t a
  r <- read a
  pure $ r === Just d

prop_upload_overwrite :: Text -> Text -> LocalPath -> Property
prop_upload_overwrite d1 d2 l = withLocalAWS $ \p a -> do
  let t = p F.</> localPath l
  liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
  liftIO $ T.writeFile t d1
  uploadWithModeOrFail Fail t a
  liftIO $ T.writeFile t d2
  uploadWithModeOrFail Overwrite t a
  r <- read a
  pure $ r === Just d2

prop_upload_fail :: Text -> LocalPath -> Property
prop_upload_fail d l = withLocalAWS $ \p a -> do
  let t = p F.</> localPath l
  liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
  liftIO $ T.writeFile t d
  uploadWithModeOrFail Fail t a
  r <- uploadWithMode Fail t a
  pure $ r === UploadError (UploadDestinationExists a)

prop_upload :: Text -> LocalPath -> Property
prop_upload d l = withLocalAWS $ \p a -> do
  let t = p F.</> localPath l
  liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
  liftIO $ T.writeFile t d
  uploadOrFail t a
  r <- read a
  pure $ r === Just d

prop_s3Sink :: T.Text -> Property
prop_s3Sink t = withAWS $ \a -> do
  let source = a { key = key a </> Key "source" }
  let target = a { key = key a </> Key "target" }
  let someText = "t"<>t -- make sure the text is not empty otherwise there is nothing to stream
  _ <- write source someText
  s <- maybe (throwM (Invariant "read after write should not fail")) pure =<< read' source
  _ <- A.sinkBody (A.RsBody s) (s3Sink target)
  r <- read target
  pure (r === Just someText)

prop_upload_multipart :: LocalPath -> Property
prop_upload_multipart l = forAll arbitrary $ \bs -> withLocalAWS $ \p a -> do
  let t = p F.</> localPath l
  liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
  liftIO $ withFile t WriteMode $ \h ->
    replicateM_ 1000 (LBS.hPut h (LBS.fromChunks . return $ (BS.concat . L.replicate 10000 $ bs)))
  uploadOrFail t a
  exists a

prop_abort_multipart :: Property
prop_abort_multipart = withMultipart $ \a i -> do
  sendMultipart "" a 1 i
  l <- listMultiparts (bucket a)
  forM_ (findMultiparts i l) $ abortCheck i (bucket a) 3
  r <- listMultiparts (bucket a)
  pure $
     (P.filter (== Just i) . fmap (^. A.muUploadId) $ l) === [Just i] .&&.
      findMultiparts i r === []
  where
    abortCheck i b n u = do
      abortMultipart b u
      r <- listMultiparts b
      unless (n <= (0 :: Int) || L.null (findMultiparts i r)) $ do
        liftIO $ threadDelay 500000
        abortCheck i b (n-1) u

prop_list_multipart :: Property
prop_list_multipart = withMultipart $ \a i -> do
  sendMultipart "" a 1 i
  l <- listMultiparts (bucket a)
  pure $ multipartExists i l

prop_list_parts :: Property
prop_list_parts = withMultipart $ \a i -> do
  sendMultipart "" a 1 i
  l2 <- listMultipartParts a i
  pure (length l2 === 1)

multipartExists :: Text -> [A.MultipartUpload] -> Property
multipartExists uploadId multiparts =
  P.count (findMultipart uploadId) multiparts === 1

findMultiparts :: Text -> [A.MultipartUpload] -> [A.MultipartUpload]
findMultiparts uploadId =
  P.filter (findMultipart uploadId)

findMultipart :: Text -> A.MultipartUpload -> Bool
findMultipart uploadId m =
  m ^. A.muUploadId == Just uploadId


prop_list :: Property
prop_list = forAll ((,) <$> elements muppets <*> elements southpark) $ \(m, s) -> withAWS $ \a -> do
  writeOrFail (withKey(</> Key m) a) ""
  writeOrFail (withKey(</> (Key s </> Key m)) a) ""
  r' <- list a
  pure $ (Just . Key <$> [m, s <> "/"]) === (removeCommonPrefix a <$> r')

prop_listObjects :: Property
prop_listObjects = forAll ((,) <$> elements muppets <*> elements southpark) $ \(m, s) -> withAWS $ \a -> do
  writeOrFail (withKey(</> Key m) a) ""
  writeOrFail (withKey(</> (Key s </> Key m)) a) ""
  (p, k) <- listObjects a
  pure $ ([Just . Key $ s <> "/"], [Just $ Key m]) === (removeCommonPrefix a <$> p, removeCommonPrefix a <$> k)

prop_list_recursively :: Property
prop_list_recursively = withAWS $ \a -> do
  writeOrFail a ""
  r' <- listRecursively (a { key = dirname $ key a })
  pure $ a `elem` r'

prop_list_forbidden_bucket = withAWS $ \_ -> do
  _ <- write (Address (Bucket "ambiata-dev-view") (Key "")) ""
  pure True

prop_download :: Text -> LocalPath -> Property
prop_download d l = withLocalAWS $ \p a -> do
  let t = p F.</> localPath  l
  writeOrFail a d
  download a t
  res <- liftIO $ T.readFile t
  pure $ res === d

prop_download_multipart :: Property
prop_download_multipart = forAll ((,,) <$> arbitrary <*> elements colours <*> elements muppets) $ \(bs, c, m) -> (BS.length bs /= 0) ==> withLocalAWS $ \p a -> do
  let t = p F.</> unpack c
  let o = p F.</> unpack m
  liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
  liftIO . D.createDirectoryIfMissing True $ F.takeDirectory o
  liftIO $ withFile t WriteMode $ \h ->
    replicateM_ 1000 (LBS.hPut h (LBS.fromChunks . return $ (BS.concat . L.replicate 10000 $ bs)))
  size <- liftIO . withFile t ReadMode $ hFileSize
  uploadOrFail t a

  let ten :: Int = 10

  multipartDownload a o (fromInteger size) (toInteger ten) 100
  b <- liftIO $ LBS.readFile t
  let b' = sha1 b
  o' <- liftIO $ LBS.readFile o
  let o'' = sha1 o'
  pure $ b' === o''
  where
    sha1 :: LBS.ByteString -> Digest SHA1
    sha1 = hashlazy

prop_write_download_overwrite :: Text -> Text -> LocalPath -> Property
prop_write_download_overwrite old new l = withLocalAWS $ \p a -> do
  let t = p F.</> localPath  l
  writeOrFail a old
  downloadWithMode Fail a t
  writeWithModeOrFail Overwrite a new
  downloadWithMode Overwrite a t
  r <- liftIO $ T.readFile t
  pure $ r === new

prop_write_download_fail :: Text -> Text -> LocalPath -> Property
prop_write_download_fail old new l = withLocalAWS $ \p a -> do
  let t = p F.</> localPath  l
  writeOrFail a old
  downloadWithMode Fail a t
  writeWithModeOrFail Overwrite a new
  (False <$ downloadWithMode Fail a t) `catchAll` (const . pure $ True)

prop_delete :: WriteMode -> Property
prop_delete w = withAWS $ \a -> do
  writeWithModeOrFail w a ""
  x <- exists a
  delete a
  y <- exists a
  pure $ (x, y) === (True, False)

prop_delete_empty :: Property
prop_delete_empty = withAWS $ \a ->
  (True <$ delete a) `catchAll` (const . pure $ False)

prop_read_write :: Text -> Property
prop_read_write d = withAWS $ \a -> do
  writeOrFail a d
  r <- read a
  pure $ r === Just d

prop_write_failure :: Text -> Property
prop_write_failure d = withAWS $ \a -> do
  writeOrFail a d
  r <- write a d
  pure $ r === WriteDestinationExists a

prop_write_overwrite :: UniquePair Text -> Property
prop_write_overwrite (UniquePair x y) = withAWS $ \a -> do
  writeWithModeOrFail Fail a x
  writeWithModeOrFail Overwrite a y
  r <- read a
  pure $ r === pure y

-- | If the object does not exist, then the behaviour should be invariant with the WriteMode
prop_write_nonexisting :: WriteMode -> Text -> Property
prop_write_nonexisting w t = withAWS $ \a -> do
  writeWithModeOrFail w a t
  r <- read a
  pure $ r === pure t

prop_on_status_ok = testAWS $
  do r <- onStatus_ (1 :: Int) handler (void (exists (Address (Bucket "ambiata-dev-view") (Key ""))))
     return (r === 1)
  where handler _ = Just 2

prop_on_status_ko = testAWS $
  do r <- onStatus_ (1 :: Int) handler (void (write missingAddress "text"))
     return (r === 2)
  where handler _ = Just 2

prop_read_empty :: Key -> Property
prop_read_empty k = ioProperty $ do
  bucket' <- testBucket
  t <- runAWSDefaultRegion . read $ Address bucket' k
  pure $ t === Nothing

----------
-- HELPERS
----------
missingAddress = Address (Bucket "ambiata-missing") (Key "m")

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
