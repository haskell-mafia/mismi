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

import "cryptohash" Crypto.Hash

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import           Data.Text hiding (copy, length)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import           Disorder.Core
import           Disorder.Corpus

import           Control.Lens hiding (elements)

import           Mismi.S3
import qualified Mismi.S3.Amazonka as A

import           P

import qualified System.Directory as D
import           System.FilePath ((</>))
import qualified System.FilePath as F
import           System.IO
import           System.IO.Error

import           Test.Mismi.Amazonka
import           Test.Mismi.S3
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Twine.Parallel (RunError (..))

import           X.Control.Monad.Trans.Either (runEitherT, eitherT)

prop_exists = testAWS $ do
  a <- newAddress
  writeOrFail a ""
  e <- exists a
  pure $ e === True

prop_exists_empty = testAWS $ do
  a <- newAddress
  not <$> exists a

prop_exists_failure = testAWS $ do
  a <- newAddress
  e <- exists a
  pure $ e === False

prop_exists_prefix k = k /= Key "" ==> testAWS $ do
  a <- newAddress
  writeOrFail (withKey (// k) a) ""
  e <- existsPrefix a
  pure $ e === True

prop_exists_prefix_missing = testAWS $ do
  a <- newAddress
  writeOrFail a ""
  e <- existsPrefix a
  pure $ e === False

prop_exists_prefix_key = testAWS $ do
  a <- newAddress
  e <- existsPrefix a
  pure $ e === False

prop_headObject = testAWS $ do
  a <- newAddress
  h <- headObject a
  pure $ h === Nothing


prop_getObjects_empty = testAWS $ do
  a <- newAddress
  objs <- getObjectsRecursively $ a
  pure $ objs === []

prop_getObjectsR d p1 p2 = p1 /= p2 ==> testAWS $ do
  root <- newAddress
  let keys = [p1, p2 // p1, p2 // p2]
  forM_ keys $ \k -> writeOrFail (withKey (// k) root) d
  objs <- getObjectsRecursively root
  pure $ on (===) L.sort ((^. A.oKey . to A.toText) <$> objs) (unKey . (//) (key root) <$> keys)

prop_getObjs = forAll ((,) <$> elements muppets <*> choose (1000, 1500)) $ \(m, n) -> testAWS $ do
  a <- newAddress
  forM_ [1..n] $ \n' -> writeOrFail (withKey(// Key (m <> pack (show n'))) a) ""
  r' <- list a
  pure $ length r' === n

prop_size d = testAWS $ do
  a <- newAddress
  writeOrFail a d
  i <- size a
  pure $ i === (Just . fromIntegral . BS.length $ T.encodeUtf8 d)

prop_size_failure = testAWS $ do
  a <- newAddress
  i <- size a
  pure $ i === Nothing

prop_size_recursively d = testAWS $ do
  a <- newAddress
  writeOrFail a d
  r <- sizeRecursively (a { key = dirname $ key a })
  pure $ r === [Sized (fromIntegral . BS.length $ T.encodeUtf8 d) a]

prop_concat =
  once . testAWS $ do
    a <- newAddress
    b <- newAddress
    c <- newAddress
    f <- newFilePath
    let
      s = f </> T.unpack "fred"
      d = f </> T.unpack "down"
      bs10k = BS.concat $ L.replicate 10000 "fred"
    liftIO $ withFile s WriteMode $ \h ->
      replicateM_ 1000 (BS.hPut h bs10k)
    uploadOrFail s a
    uploadOrFail s b

    eitherT (fail . show . renderConcatError) pure $ concatMultipart Fail 1 [a, b] c

    eitherT (fail . show) pure $ download c d
    s' <- liftIO $ LBS.readFile s
    d' <- liftIO $ LBS.readFile d
    pure $ (sha1 (LBS.concat [s', s']) === sha1 d')

prop_concat_empty_input =
  testAWS $ do
    a <- newAddress
    r <- runEitherT $ concatMultipart Fail 1 [] a
    pure $ case r of
      Left NoInputFiles ->
        property True
      _ ->
        failWith "concat didn't fail correctly"

prop_concat_empty_input_files =
  testAWS $ do
    a <- newAddress
    b <- newAddress
    writeOrFail a ""
    r <- runEitherT $ concatMultipart Fail 1 [a] b
    pure $ case r of
      Left NoInputFilesWithData ->
        property True
      _ ->
        failWith "concat didn't fail correctly"

prop_copy t = testAWS $ do
  a <- newAddress
  b <- newAddress
  writeOrFail a t
  eitherT (fail . T.unpack . renderCopyError) pure $ copy a b
  a' <- read a
  b' <- read b
  pure $ a' === b'

prop_copy_escape t = testAWS $ do
  a <- fmap (withKey ((Key "%%~") //)) newAddress
  b <- newAddress
  writeOrFail a t
  eitherT (fail . T.unpack . renderCopyError) pure $ copy a b
  a' <- read a
  b' <- read b
  pure $ a' === b'

prop_copy_missing = testAWS $ do
  a <- newAddress
  r <- runEitherT $ copy a a
  pure $ case r of
    Left (CopySourceMissing b) ->
      a === b
    _ ->
      failWith "Copy didn't fail correctly"

prop_copy_overwrite t t' = testAWS $ do
  a <- newAddress
  b <- newAddress
  writeOrFail a t
  writeOrFail b t'
  eitherT (fail . T.unpack . renderCopyError) pure $ copyWithMode Overwrite a b
  b' <- read b
  pure $ b' === Just t

prop_copy_fail t = testAWS $ do
  a <- newAddress
  b <- newAddress
  writeOrFail a t
  writeOrFail b t
  r <- runEitherT $ copyWithMode Fail a b
  pure $ case r of
    Left (CopyDestinationExists z) ->
      b === z
    _ ->
      failWith "Copy didn't failure correctly"

prop_copy_multipart = forAll ((,,) <$> arbitrary <*> elements colours <*> elements muppets) $ \(bs, c, m) -> (BS.length bs /= 0) ==> testAWS $ do
  f <- newFilePath
  a' <- newAddress
  let a = withKey (// Key c) a'
      b = withKey (// Key m) a'
      s = f </> T.unpack c
      d = f </> T.unpack m
  -- create large file to copy
  liftIO $ D.createDirectoryIfMissing True f
  liftIO $ withFile s WriteMode $ \h ->
    replicateM_ 1000 (LBS.hPut h (LBS.fromChunks . return $ (BS.concat . L.replicate 10000 $ bs)))
  liftIO . putStrLn $ "Generated file"

  uploadOrFail s a
  liftIO . putStrLn $ "Uploaded file"

  liftIO . putStrLn $ "Running copy ..."
  eitherT (fail . T.unpack . renderCopyError) pure $ copy a b

  liftIO . putStrLn $ "Done copy"
  -- compare
  eitherT (fail . show) pure $ download b d
  liftIO . putStrLn $ "Done download"

  s' <- liftIO $ LBS.readFile s
  d' <- liftIO $ LBS.readFile d
  pure $ (sha1 s' === sha1 d')

prop_move t = testAWS $ do
  s <- newAddress
  d <- newAddress
  writeOrFail s t
  eitherT (fail . T.unpack . renderCopyError) pure $ move s d
  es <- exists s
  ed <- exists d
  pure $ (es, ed) === (False, True)

prop_upload_mode d l m = testAWS $ do
  p <- newFilePath
  a <- newAddress
  let t = p </> localPath l
  liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
  liftIO $ T.writeFile t d
  uploadWithModeOrFail m t a
  r <- read a
  pure $ r === Just d

prop_upload_overwrite d1 d2 l = testAWS $ do
  p <- newFilePath
  a <- newAddress
  let t = p </> localPath l
  liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
  liftIO $ T.writeFile t d1
  uploadWithModeOrFail Fail t a
  liftIO $ T.writeFile t d2
  uploadWithModeOrFail Overwrite t a
  r <- read a
  pure $ r === Just d2

prop_upload_fail d l = testAWS $ do
  p <- newFilePath
  a <- newAddress
  let t = p </> localPath l
  liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
  liftIO $ T.writeFile t d
  uploadWithModeOrFail Fail t a
  r <- runEitherT $ uploadWithMode Fail t a
  pure $ case r of
    Left (UploadDestinationExists _) ->
      property True
    _ ->
      failWith "Upload succeded but should have failed"

prop_upload d l = testAWS $ do
  p <- newFilePath
  a <- newAddress
  let t = p </> localPath l
  liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
  liftIO $ T.writeFile t d
  uploadOrFail t a
  r <- read a
  pure $ r === Just d

prop_upload_multipart l = forAll arbitrary $ \bs -> testAWS $ do
  p <- newFilePath
  a <- newAddress
  let t = p </> localPath l
  liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
  liftIO $ withFile t WriteMode $ \h ->
    replicateM_ 1000 (LBS.hPut h (LBS.fromChunks . return $ (BS.concat . L.replicate 10000 $ bs)))
  uploadOrFail t a
  exists a

prop_abort_multipart = testAWS $ do
  (a, i) <- newMultipart
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

prop_list_multipart = testAWS $ do
  (a, i) <- newMultipart
  sendMultipart "" a 1 i
  l <- listMultiparts (bucket a)
  pure $ multipartExists i l

prop_list_parts = testAWS $ do
  (a, i) <- newMultipart
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

prop_list = forAll ((,) <$> elements muppets <*> elements southpark) $ \(m, s) -> testAWS $ do
  a <- newAddress
  writeOrFail (withKey (// Key m) a) ""
  writeOrFail (withKey (// (Key s // Key m)) a) ""
  r' <- list a
  pure $ (Just . Key <$> [m, s <> "/"]) === (removeCommonPrefix a <$> r')

prop_listObjects = forAll ((,) <$> elements muppets <*> elements southpark) $ \(m, s) -> testAWS $ do
  a <- newAddress
  writeOrFail (withKey (// Key m) a) ""
  writeOrFail (withKey (// (Key s // Key m)) a) ""
  (p, k) <- listObjects a
  pure $ ([Just . Key $ s <> "/"], [Just $ Key m]) === (removeCommonPrefix a <$> p, removeCommonPrefix a <$> k)

prop_list_recursively = testAWS $ do
  a <- newAddress
  writeOrFail a ""
  r' <- listRecursively (a { key = dirname $ key a })
  pure $ a `elem` r'

prop_list_forbidden_bucket = testAWS $ do
  _ <- write (Address (Bucket "ambiata-dev-view") (Key "")) ""
  pure $ True

prop_download d l = testAWS $ do
  p <- newFilePath
  a <- newAddress
  let t = p </> localPath l
  writeOrFail a d
  r <- runEitherT $ download a t
  res <- liftIO $ T.readFile t
  pure $ (isRight r, res) === (True, d)

prop_download_multipart :: Property
prop_download_multipart = forAll ((,,) <$> arbitrary <*> elements colours <*> elements muppets) $ \(bs, c, m) ->
  (BS.length bs /= 0) ==> testAWS $ do
    p <- newFilePath
    a <- newAddress
    let t = p </> unpack c
    let o = p </> unpack m
    liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
    liftIO . D.createDirectoryIfMissing True $ F.takeDirectory o
    liftIO $ withFile t WriteMode $ \h ->
      replicateM_ 1000 (LBS.hPut h (LBS.fromChunks . return $ (BS.concat . L.replicate 10000 $ bs)))
    sz <- liftIO . withFile t ReadMode $ hFileSize
    uploadOrFail t a

    let ten :: Int = 10

    r <- runEitherT $ multipartDownload a o (fromInteger sz) (toInteger ten) 100
    b <- liftIO $ LBS.readFile t

    let b' = sha1 b
    o' <- liftIO $ LBS.readFile o
    let o'' = sha1 o'
    pure $ (isRight r, b') === (True, o'')


prop_write_download_overwrite old new l = testAWS $ do
  p <- newFilePath
  a <- newAddress
  let t = p </> localPath l
  writeOrFail a old
  x <- runEitherT $ downloadWithMode Fail a t
  writeWithModeOrFail Overwrite a new
  y <- runEitherT $ downloadWithMode Overwrite a t
  r <- liftIO $ T.readFile t
  pure $ (isRight x, isRight y, r) === (True, True, new)

prop_write_download_fail old new l = testAWS $ do
  p <- newFilePath
  a <- newAddress
  let t = p </> localPath l
  writeOrFail a old
  x <- runEitherT $ downloadWithMode Fail a t
  writeWithModeOrFail Overwrite a new
  y <- runEitherT (downloadWithMode Fail a t)
  pure $ (isRight x, isLeft y) === (True, True)

prop_delete w = testAWS $ do
  a <- newAddress
  writeWithModeOrFail w a ""
  x <- exists a
  delete a
  y <- exists a
  pure $ (x, y) === (True, False)

prop_delete_empty = testAWS $ do
  a <- newAddress
  (True <$ delete a) `catchAll` (const . pure $ False)

prop_read_write d = testAWS $ do
  a <- newAddress
  writeOrFail a d
  r <- read a
  pure $ r === Just d

prop_write_failure d = testAWS $ do
  a <- newAddress
  writeOrFail a d
  r <- write a d
  pure $ r === WriteDestinationExists a

prop_write_overwrite (UniquePair x y) = testAWS $ do
  a <- newAddress
  writeWithModeOrFail Fail a x
  writeWithModeOrFail Overwrite a y
  r <- read a
  pure $ r === pure y

prop_sync_overwrite = forAll (elements muppets) $ \m -> testAWS $ do
  a <- newAddress
  b <- newAddress
  createSmallFiles a m 10
  x <- runEitherT $ syncWithMode OverwriteSync a b 1
  y <- runEitherT $ syncWithMode OverwriteSync a b 1
  mapM_ (\e -> exists e >>= \e' -> when (e' == False) (throwM . userError $ "Output files do not exist")) (files b m 10)
  pure $ (isRight x, isRight y) === (True, True)

prop_sync_fail = forAll (elements muppets) $ \m -> testAWS $ do
  a <- newAddress
  b <- newAddress
  createSmallFiles a m 1
  x <- runEitherT $ syncWithMode FailSync a b 1
  y <- runEitherT $ syncWithMode FailSync a b 1
  r <- pure $ case y of
    (Left (SyncError (WorkerError (OutputExists q)))) ->
      q == withKey (// Key (m <> "-1")) b
    _ ->
      False
  pure $ (isRight x, r) === (True, True)

-- | If the object does not exist, then the behaviour should be invariant with the WriteMode
prop_write_nonexisting w d = testAWS $ do
  a <- newAddress
  writeWithModeOrFail w a d
  r <- read a
  pure $ r === pure d

prop_write_grant t = testAWS $ do
  a <- newAddress
  writeOrFail a t
  grantReadAccess a $ ReadGrant "id=e3abd0cceaecbd471c3eaaa47bb722bf199296c5e41c9ee4222877cc91b536fc"
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

sha1 :: LBS.ByteString -> Digest SHA1
sha1 =
  hashlazy

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
