{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Mismi.S3.Commands where

import qualified Aws.S3 as S3

import           Control.Monad.Catch (catchAll)
import           Control.Monad.IO.Class

import           Crypto.Hash

import           Data.Bool
import qualified Data.ByteString.Lazy as BSL
import           Data.List (sort)
import qualified Data.List as L
import           Data.Text hiding (copy, length)
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import           Disorder.Corpus
import           Disorder.Core.IO
import           Disorder.Core.UniquePair hiding (snd)

import           Mismi.S3.Control
import           Mismi.S3.Data
import           Mismi.S3.Default

import           P

import qualified System.Directory as D
import qualified System.FilePath as F
import           System.IO
import           System.IO.Temp

import           Test.Mismi.S3
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


prop_exists :: Property
prop_exists = testS3 $ \a -> do
  write a ""
  exists a

prop_exists_empty :: Property
prop_exists_empty = testS3 $ \a ->
  not <$> exists a

prop_delete :: WriteMode -> Property
prop_delete w = testS3 $ \a -> do
  writeWithMode w a ""
  x <- exists a
  delete a
  y <- exists a
  pure $ (x, y) === (True, False)

prop_delete_empty :: Property
prop_delete_empty = testS3 $ \a ->
  (True <$ delete a) `catchAll` (const . pure $ False)

prop_read_write :: Text -> Property
prop_read_write d = testS3 $ \a -> do
  write a d
  r <- read a
  pure $ r === Just d

prop_write_download :: Text -> LocalPath -> Property
prop_write_download d l = testLocalS3 $ \p a -> do
  let t = p F.</> localPath  l
  write a d
  download a t
  res <- liftIO $ T.readFile t
  pure $ res === d

prop_write_download_overwrite :: Text -> Text -> LocalPath -> Property
prop_write_download_overwrite old new l = testLocalS3 $ \p a -> do
  let t = p F.</> localPath  l
  write a old
  downloadWithMode Fail a t
  writeWithMode Overwrite a new
  downloadWithMode Overwrite a t
  r <- liftIO $ T.readFile t
  pure $ r === new

prop_write_download_fail :: Text -> Text -> LocalPath -> Property
prop_write_download_fail old new l = testLocalS3 $ \p a -> do
  let t = p F.</> localPath  l
  write a old
  downloadWithMode Fail a t
  writeWithMode Overwrite a new
  (False <$ downloadWithMode Fail a t) `catchAll` (const . pure $ True)

prop_download_multipart :: Property
prop_download_multipart = forAll ((,,) <$> arbitrary <*> elements colours <*> elements muppets) $ \(bs, c, m) -> (BS.length bs /= 0) ==> testLocalS3 $ \p a -> do
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
  b <- liftIO $ BSL.readFile t
  let b' = sha1 b
  o' <- liftIO $ BSL.readFile o
  let o'' = sha1 o'
  pure $ b' === o''

sha1 :: BSL.ByteString -> Digest SHA1
sha1 = hashlazy

prop_upload :: Text -> LocalPath -> Property
prop_upload d l = testLocalS3 $ \p a -> do
  let t = p F.</> localPath l
  liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
  liftIO $ T.writeFile t d
  upload t a
  r <- read a
  pure $ r === Just d

prop_upload_multipart :: LocalPath -> Property
prop_upload_multipart l = forAll arbitrary $ \bs -> testLocalS3 $ \p a -> do
  let t = p F.</> localPath l
  liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
  liftIO $ withFile t WriteMode $ \h ->
    replicateM_ 1000 (LBS.hPut h (LBS.fromChunks . return $ (BS.concat . L.replicate 10000 $ bs)))
  upload t a
  exists a

prop_write_failure :: Text -> Property
prop_write_failure d = testS3 $ \a -> do
  write a d
  (False <$ write a d) `catchAll` (const . pure $ True)

prop_write_overwrite :: UniquePair Text -> Property
prop_write_overwrite (UniquePair x y) = testS3 $ \a -> do
  writeWithMode Fail a x
  writeWithMode Overwrite a y
  r <- read a
  pure $ r === pure y

-- |
-- If the object does not exist, then the behaviour should be invariant with the WriteMode
--
prop_write_nonexisting :: WriteMode -> Text -> Property
prop_write_nonexisting w t = testS3 $ \a -> do
  writeWithMode w a t
  r <- read a
  pure $ r === pure t

prop_read_empty :: Key -> Property
prop_read_empty k = ioProperty $ do
  bucket' <- testBucket
  t <- runS3WithDefaults . read $ Address bucket' k
  pure $ t === Nothing

prop_getObjects_empty :: Property
prop_getObjects_empty = testS3 $ \a -> do
  objs <- getObjectsRecursively $ a
  pure $ fmap S3.objectKey objs === []

prop_getObjectsR :: Text -> Key -> Key -> Property
prop_getObjectsR t p1 p2 = p1 /= p2 ==> testS3 $ \root -> do
  let keys = [p1, p2 </> p1, p2 </> p2]
  forM_ keys $ \k -> write (withKey (</> k) root) t
  objs <- getObjectsRecursively root
  pure $ on (===) sort (S3.objectKey <$> objs) (unKey . (</>) (key root) <$> keys)

prop_listRecursively :: Property
prop_listRecursively = testS3 $ \a -> do
  write a ""
  r' <- listRecursively (a { key = dirname $ key a })
  pure $ a `elem` r'

prop_list :: Property
prop_list = forAll ((,) <$> elements muppets <*> elements southpark) $ \(m, s) -> testS3 $ \a -> do
  write (withKey(</> Key m) a) ""
  write (withKey(</> (Key s </> Key m)) a) ""
  r' <- list a
  pure $ (Just . Key <$> [m, s <> "/"]) === (removeCommonPrefix a <$> r')

prop_listObjects :: Property
prop_listObjects = forAll ((,) <$> elements muppets <*> elements southpark) $ \(m, s) -> testS3 $ \a -> do
  write (withKey(</> Key m) a) ""
  write (withKey(</> (Key s </> Key m)) a) ""
  (p, k) <- listObjects a
  pure $ ([Just . Key $ s <> "/"], [Just $ Key m]) === (removeCommonPrefix a <$> p, removeCommonPrefix a <$> k)

prop_getObjs :: Property
prop_getObjs = forAll ((,) <$> elements muppets <*> choose (1000, 1500)) $ \(m, n) -> once . testS3 $ \a -> do
  forM_ [1..n] $ \n' -> write (withKey(</> Key (m <> pack (show n'))) a) ""
  r' <- list a
  pure $ length r' === n

prop_copy :: Text -> Token -> Property
prop_copy t d' = testS3 $ \s ->
  withToken d' $ \d -> do
    write s t
    copy s d
    es <- exists s
    ed <- exists d
    pure $ (es, ed) === (True, True)

prop_move :: Text -> Token -> Property
prop_move t d' = testS3 $ \s ->
  withToken d' $ \d -> do
    write s t
    move s d
    es <- exists s
    ed <- exists d
    pure $ (es, ed) === (False, True)

prop_size :: Text -> Property
prop_size t = testS3 $ \a -> do
  write a t
  i <- getSize a
  pure $ i === (Just . BS.length $ T.encodeUtf8 t)

prop_size_failure :: Property
prop_size_failure = testS3 $ \a -> do
  i <- getSize a
  pure $ i === Nothing

testLocalS3 :: Testable a => (FilePath -> Address -> S3Action a) -> Property
testLocalS3 f =
  property $ \t -> testIO . withSystemTempDirectory "mismi" $ \p ->
    runS3WithDefaults . withToken t $ \a ->
      f p a

testS3 :: Testable a => (Address -> S3Action a) -> Property
testS3 f =
  property $ \t -> testIO .
    runS3WithDefaults . withToken t $ \a ->
      f a

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
