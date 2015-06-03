{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Mismi.S3.Commands where

import qualified Aws.S3 as S3

import           Control.Monad.Catch (catchAll)

import           Control.Monad.IO.Class

import           Data.Bool
import           Data.List (sort)
import qualified Data.List as L
import           Data.Text as T hiding (copy, length)
import qualified Data.Text.IO as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import           Disorder.Corpus
import           Disorder.Core.IO

import           Mismi.S3.Control
import           Mismi.S3.Commands
import           Mismi.S3.Data

import           Disorder.Core.UniquePair hiding (snd)

import qualified System.Directory as D
import qualified System.FilePath as F
import           System.IO
import           System.IO.Temp

import           Test.Mismi
import           Test.Mismi.S3
import           Test.QuickCheck.Monadic

prop_exists :: Token -> Property
prop_exists t = monadicIO $ do
  r <- run $ do
    runS3WithDefaults . withToken t $ \a -> do
      write a ""
      exists a
  stop $ r === True

prop_exists_empty :: Token -> Property
prop_exists_empty t = monadicIO $ do
  r <- run $
    runS3WithDefaults . withToken t $ \a ->
      exists a
  stop $ r === False

prop_delete :: WriteMode -> Token -> Property
prop_delete w t = monadicIO $ do
  r <- run $
    runS3WithDefaults . withToken t $ \a -> do
      writeWithMode w a ""
      x <- exists a
      delete a
      y <- exists a
      pure (x, y)
  stop $ r == (True, False)

prop_delete_empty :: Token -> Property
prop_delete_empty t = monadicIO $ do
    r <- run $ (
      runS3WithDefaults . withToken t $ \a -> do
        delete a
        pure $ True)
         `catchAll`
         (\(_) -> pure $ False)
    stop $ r === True

prop_read_write :: Token -> Text -> Property
prop_read_write t d = monadicIO $ do
    r <- run $
      runS3WithDefaults . withToken t $ \a -> do
        write a d
        read a
    stop $ r === Just d

prop_write_download :: Token -> Text -> LocalPath -> Property
prop_write_download tt d l = monadicIO $ do
    r <- run $
      withSystemTempDirectory "mismi" $ \p ->
        runS3WithDefaults . withToken tt $ \a -> do
          write a d
          let t = p F.</> localPath  l
          download a t
          liftIO $ T.readFile t
    stop $ r === d

prop_write_download_overwrite :: Token -> Text -> Text -> LocalPath -> Property
prop_write_download_overwrite tt d e l = monadicIO $ do
    r <- run $
      withSystemTempDirectory "mismi" $ \p ->
        runS3WithDefaults . withToken tt $ \a -> do
          write a d
          let t = p F.</> localPath  l
          downloadWithMode Fail a t
          write a e
          downloadWithMode Overwrite a t
          liftIO $ T.readFile t
    stop $ r === e

prop_write_download_fail :: Token -> Text -> Text -> LocalPath -> Property
prop_write_download_fail tt d e l = monadicIO $ do
    r <- run $
      withSystemTempDirectory "mismi" $ \p ->
        runS3WithDefaults . withToken tt $ \a -> (do
          write a d
          let t = p F.</> localPath  l
          downloadWithMode Fail a t
          write a e
          downloadWithMode Fail a t
          pure False) `catchAll` (const . pure $ True)
    stop $ r

prop_upload :: Token -> Text -> LocalPath -> Property
prop_upload tt d l = monadicIO $ do
    r <- run $ do
      withSystemTempDirectory "mismi" $ \p -> do
        runS3WithDefaults . withToken tt $ \a -> do
          let t = p F.</> localPath l
          liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
          liftIO $ T.writeFile t d
          upload t a
          read a
    stop $ r === Just d

prop_upload_multipart :: Token -> LocalPath -> Property
prop_upload_multipart tt l = forAll arbitrary $ \bs -> monadicIO $ do
  r <- run $ do
    withSystemTempDirectory "mismi" $ \p -> do
      runS3WithDefaults . withToken tt $ \a -> do
        let t = p F.</> localPath l
        liftIO . D.createDirectoryIfMissing True $ F.takeDirectory t
        liftIO $ withFile t WriteMode $ \h ->
          replicateM_ 1000 (LBS.hPut h (LBS.fromChunks . return $ (BS.concat . L.replicate 10000 $ bs)))
        upload t a
        exists a
  assert $ r == True

prop_write_failure :: Token -> Text -> Property
prop_write_failure t d = monadicIO $ do
    r <- run $ (
      runS3WithDefaults . withToken t $ \a -> do
        write a d
        write a d
        pure False)
         `catchAll`
         (\(_) -> pure $ True)
    stop $ r === True

prop_write_overwrite :: Token -> UniquePair Text -> Property
prop_write_overwrite t (UniquePair x y) = monadicIO $ do
    r <- run $
        runS3WithDefaults . withToken t $ \a -> do
            writeWithMode Fail a x
            writeWithMode Overwrite a y
            read a
    stop $ r === pure y

-- |
-- If the object does not exist, then the behaviour should be invariant with the WriteMode
--
prop_write_nonexisting :: WriteMode -> Token -> Text -> Property
prop_write_nonexisting w tt t = monadicIO $ do
    r <- run $
        runS3WithDefaults . withToken tt $ \a -> do
            writeWithMode w a t
            read a
    stop $ r === pure t

prop_read_empty :: Key -> Property
prop_read_empty k = ioProperty $ do
  bucket' <- testBucket
  t <- runS3WithDefaults . read $ Address bucket' k
  pure $ t === Nothing

prop_getObjects_empty :: Token -> Property
prop_getObjects_empty t = ioProperty . runS3WithDefaults . withToken t $ \a -> do
  objs <- getObjectsRecursively $ a
  pure $ fmap S3.objectKey objs === []

prop_getObjectsR :: Token -> Text -> Key -> Key -> Property
prop_getObjectsR token t p1 p2 = p1 /= p2 ==> ioProperty .
  runS3WithDefaults . withToken token $ \root -> do
    let keys = [p1, p2 </> p1, p2 </> p2]
    forM_ keys $ \k -> write (withKey (</> k) root) t
    objs <- getObjectsRecursively root
    pure $ on (===) sort (S3.objectKey <$> objs) (unKey . (</>) (key root) <$> keys)

prop_listRecursively :: Token -> Property
prop_listRecursively t = monadicIO $
  stop =<< (run . runS3WithDefaults . withToken t $ \a -> do
    write a ""
    r' <- listRecursively (a { key = dirname $ key a })
    pure $ a `elem` r')

prop_list :: Token -> Property
prop_list t = forAll ((,) <$> elements muppets <*> elements southpark) $ \(m, s) -> monadicIO $
  stop =<< (run . runS3WithDefaults . withToken t $ \a -> do
    write (withKey(</> Key m) a) ""
    write (withKey(</> (Key s </> Key m)) a) ""
    r' <- list a
    pure $ [m, s <> "/"] === (replace (addressToText a <> "/") "" . addressToText <$> r'))

prop_getObjs :: Token -> Property
prop_getObjs t = forAll ((,) <$> elements muppets <*> choose (1000, 1500)) $ \(m, n) -> once . monadicIO $
  stop =<< (run . runS3WithDefaults . withToken t $ \a -> do
    forM_ [1..n] $ \n' -> write (withKey(</> Key (m <> pack (show n'))) a) ""
    r' <- list a
    pure $ length r' === n)

prop_copy :: Text -> Token -> Token -> Property
prop_copy t s' d' = testIO $ do
  runS3WithDefaults . withToken s' $ \s ->
    withToken d' $ \d -> do
      write s t
      copy s d
      es <- exists s
      ed <- exists d
      pure $ (es, ed) === (True, True)

prop_move :: Text -> Token -> Token -> Property
prop_move t s' d' = testIO $ do
  runS3WithDefaults . withToken s' $ \s ->
    withToken d' $ \d -> do
      write s t
      move s d
      es <- exists s
      ed <- exists d
      pure $ (es, ed) === (False, True)


return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
