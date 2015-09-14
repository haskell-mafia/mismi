{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Mismi.S3.Commands where

import           Control.Monad.Catch
import           Control.Monad.IO.Class

import           Crypto.Hash

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import           Data.Text hiding (copy, length)
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import           Disorder.Core
import           Disorder.Corpus

import           Control.Lens hiding (elements)

import           Mismi.S3
import qualified Mismi.S3 as A
import qualified Mismi.S3.Commands as C

import           P

import qualified System.Directory as D
import qualified System.FilePath as F
import           System.IO

import           Test.Mismi.Amazonka
import           Test.Mismi.S3
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


prop_exists :: Property
prop_exists = withAWS $ \a -> do
  write a ""
  e <- exists a
  pure $ e === True

prop_exists_empty :: Property
prop_exists_empty = withAWS $ \a ->
  not <$> exists a

prop_exists_failure :: Property
prop_exists_failure = withAWS $ \a -> do
  e <- exists a
  pure $ e === False


prop_headObject :: Property
prop_headObject = withAWS $ \a -> do
  h <- C.headObject a
  pure $ h === Nothing


prop_getObjects_empty :: Property
prop_getObjects_empty = withAWS $ \a -> do
  objs <- getObjectsRecursively $ a
  pure $ objs === []

prop_getObjectsR :: Text -> Key -> Key -> Property
prop_getObjectsR t p1 p2 = p1 /= p2 ==> withAWS $ \root -> do
  let keys = [p1, p2 </> p1, p2 </> p2]
  forM_ keys $ \k -> write (withKey (</> k) root) t
  objs <- getObjectsRecursively root
  pure $ on (===) L.sort ((^. C.oKey . to toText) <$> objs) (unKey . (</>) (key root) <$> keys)

prop_getObjs :: Property
prop_getObjs = forAll ((,) <$> elements muppets <*> choose (1000, 1500)) $ \(m, n) -> withAWS $ \a -> A.once $ do
  forM_ [1..n] $ \n' -> write (withKey(</> Key (m <> pack (show n'))) a) ""
  r' <- list a
  pure $ length r' === n

prop_size :: Text -> Property
prop_size t = withAWS $ \a -> do
  write a t
  i <- getSize a
  pure $ i === (Just . BS.length $ T.encodeUtf8 t)

prop_size_failure :: Property
prop_size_failure = withAWS $ \a -> do
  i <- getSize a
  pure $ i === Nothing

prop_copy :: Text -> Property
prop_copy t = withAWS' $ \a b -> do
  write a t
  copy a b
  a' <- read a
  b' <- read b
  pure $ a' === b'

prop_copy_overwrite :: Text -> Text -> Property
prop_copy_overwrite t t' = withAWS' $ \a b -> do
  write a t
  write b t'
  copyWithMode Overwrite a b
  b' <- read b
  pure $ b' === Just t

prop_copy_fail :: Text -> Property
prop_copy_fail t = withAWS' $ \a b -> do
  write a t
  write b t
  (False <$ copyWithMode Fail a b) `catchAll` (const . pure $ True)


prop_move :: Text -> Token -> Property
prop_move t d' = withAWS $ \s ->
  withToken d' $ \d -> do
    write s t
    move s d
    es <- exists s
    ed <- exists d
    pure $ (es, ed) === (False, True)

prop_upload_mode :: Text -> LocalPath -> WriteMode -> Property
prop_upload_mode d l m = withLocalAWS $ \p a -> do
  let t = p F.</> localPath l
  liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
  liftIO $ T.writeFile t d
  uploadWithMode m t a
  r <- read a
  pure $ r === Just d

prop_upload_overwrite :: Text -> Text -> LocalPath -> Property
prop_upload_overwrite d1 d2 l = withLocalAWS $ \p a -> do
  let t = p F.</> localPath l
  liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
  liftIO $ T.writeFile t d1
  uploadWithMode Fail t a
  liftIO $ T.writeFile t d2
  uploadWithMode Overwrite t a
  r <- read a
  pure $ r === Just d2

prop_upload_fail :: Text -> LocalPath -> Property
prop_upload_fail d l = withLocalAWS $ \p a -> do
  let t = p F.</> localPath l
  liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
  liftIO $ T.writeFile t d
  uploadWithMode Fail t a
  (False <$ uploadWithMode Fail t a) `catchAll` (const . pure $ True)

prop_upload :: Text -> LocalPath -> Property
prop_upload d l = withLocalAWS $ \p a -> do
  let t = p F.</> localPath l
  liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
  liftIO $ T.writeFile t d
  upload t a
  r <- read a
  pure $ r === Just d

prop_upload_multipart :: LocalPath -> Property
prop_upload_multipart l = forAll arbitrary $ \bs -> withLocalAWS $ \p a -> do
  let t = p F.</> localPath l
  liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
  liftIO $ withFile t WriteMode $ \h ->
    replicateM_ 1000 (LBS.hPut h (LBS.fromChunks . return $ (BS.concat . L.replicate 10000 $ bs)))
  upload t a
  exists a

prop_abort_multipart :: Property
prop_abort_multipart = withMultipart $ \a i -> do
  sendMultipart "" a 1 i
  l <- listMultiparts (bucket a)
  forM_ (findMultiparts i l) $ abortMultipart (bucket a)
  r <- listMultiparts (bucket a)
  pure $
     (filter (== Just i) . fmap (^. muUploadId) $ l) === [Just i] .&&.
      findMultiparts i r === []

prop_list_multipart :: Property
prop_list_multipart = withMultipart $ \a i -> do
  sendMultipart "" a 1 i
  l <- listMultiparts (bucket a)
  pure $ multipartExists i l

prop_list_parts :: Property
prop_list_parts = withMultipart $ \a i -> do
  sendMultipart "" a 1 i
  l2 <- retryAction $ C.listMultipartParts a i
  pure (length l2 === 1)

multipartExists :: Text -> [MultipartUpload] -> Property
multipartExists uploadId multiparts =
  P.count (findMultipart uploadId) multiparts === 1

findMultiparts :: Text -> [MultipartUpload] -> [MultipartUpload]
findMultiparts uploadId =
  P.filter (findMultipart uploadId)

findMultipart :: Text -> MultipartUpload -> Bool
findMultipart uploadId m =
  m ^. muUploadId == Just uploadId


prop_list :: Property
prop_list = forAll ((,) <$> elements muppets <*> elements southpark) $ \(m, s) -> withAWS $ \a -> do
  write (withKey(</> Key m) a) ""
  write (withKey(</> (Key s </> Key m)) a) ""
  r' <- list a
  pure $ (Just . Key <$> [m, s <> "/"]) === (removeCommonPrefix a <$> r')

prop_listObjects :: Property
prop_listObjects = forAll ((,) <$> elements muppets <*> elements southpark) $ \(m, s) -> withAWS $ \a -> do
  write (withKey(</> Key m) a) ""
  write (withKey(</> (Key s </> Key m)) a) ""
  (p, k) <- listObjects a
  pure $ ([Just . Key $ s <> "/"], [Just $ Key m]) === (removeCommonPrefix a <$> p, removeCommonPrefix a <$> k)

prop_list_recursively :: Property
prop_list_recursively = withAWS $ \a -> do
  write a ""
  r' <- listRecursively (a { key = dirname $ key a })
  pure $ a `elem` r'


prop_download :: Text -> LocalPath -> Property
prop_download d l = withLocalAWS $ \p a -> do
  let t = p F.</> localPath  l
  write a d
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
  upload t a

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
  write a old
  downloadWithMode Fail a t
  writeWithMode Overwrite a new
  downloadWithMode Overwrite a t
  r <- liftIO $ T.readFile t
  pure $ r === new

prop_write_download_fail :: Text -> Text -> LocalPath -> Property
prop_write_download_fail old new l = withLocalAWS $ \p a -> do
  let t = p F.</> localPath  l
  write a old
  downloadWithMode Fail a t
  writeWithMode Overwrite a new
  (False <$ downloadWithMode Fail a t) `catchAll` (const . pure $ True)


prop_delete :: WriteMode -> Property
prop_delete w = withAWS $ \a -> do
  writeWithMode w a ""
  x <- exists a
  delete a
  y <- exists a
  pure $ (x, y) === (True, False)

prop_delete_empty :: Property
prop_delete_empty = withAWS $ \a ->
  (True <$ delete a) `catchAll` (const . pure $ False)


prop_read_write :: Text -> Property
prop_read_write d = withAWS $ \a -> do
  write a d
  r <- read a
  pure $ r === Just d

prop_write_failure :: Text -> Property
prop_write_failure d = withAWS $ \a -> do
  write a d
  (False <$ write a d) `catchAll` (const . pure $ True)

prop_write_overwrite :: UniquePair Text -> Property
prop_write_overwrite (UniquePair x y) = withAWS $ \a -> do
  writeWithMode Fail a x
  writeWithMode Overwrite a y
  r <- read a
  pure $ r === pure y

-- |
-- If the object does not exist, then the behaviour should be invariant with the WriteMode
--
prop_write_nonexisting :: WriteMode -> Text -> Property
prop_write_nonexisting w t = withAWS $ \a -> do
  writeWithMode w a t
  r <- read a
  pure $ r === pure t

prop_read_empty :: Key -> Property
prop_read_empty k = ioProperty $ do
  bucket' <- testBucket
  t <- runAWSDefaultRegion . read $ Address bucket' k
  pure $ t === Nothing


return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
