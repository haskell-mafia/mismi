{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Mismi.S3.CommandsTest where

import qualified Aws.S3 as S3

import           Control.Monad.Catch (catchAll)

import           Control.Monad.IO.Class

import           Data.Bool
import           Data.List (sort)
import           Data.Text as T
import qualified Data.Text.IO as T

import           Mismi.S3.Control
import           Mismi.S3.Commands
import           Mismi.S3.Data
import           Mismi.Test
import           Mismi.Test.S3

import           Orphanarium.Core.UniquePair

import qualified System.FilePath as F
import           System.IO.Temp

import           Test.QuickCheck.Monadic

prop_exists :: Token -> Property
prop_exists t = monadicIO $ do
  r <- run $ do
    runS3WithDefaults . withToken t $ \a -> do
      write Fail a ""
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
      write w a ""
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
        write Fail a d
        read a
    stop $ r === Just d

prop_write_download :: Token -> Text -> LocalPath -> Property
prop_write_download tt d l = monadicIO $ do
    r <- run $
      withSystemTempDirectory "mismi" $ \p ->
        runS3WithDefaults . withToken tt $ \a -> do
          write Fail a d
          let t = p F.</> localPath  l
          download a t
          liftIO $ T.readFile t
    stop $ r === d

prop_write_failure :: Token -> Text -> Property
prop_write_failure t d = monadicIO $ do
    r <- run $ (
      runS3WithDefaults . withToken t $ \a -> do
        write Fail a d
        write Fail a d
        pure False)
         `catchAll`
         (\(_) -> pure $ True)
    stop $ r === True

prop_write_overwrite :: Token -> UniquePair Text -> Property
prop_write_overwrite t (UniquePair x y) = monadicIO $ do
    r <- run $
        runS3WithDefaults . withToken t $ \a -> do
            write Fail a x
            write Overwrite a y
            read a
    stop $ r === pure y

-- |
-- If the object does not exist, then the behaviour should be invariant with the WriteMode
--
prop_write_nonexisting :: WriteMode -> Token -> Text -> Property
prop_write_nonexisting w tt t = monadicIO $ do
    r <- run $
        runS3WithDefaults . withToken tt $ \a -> do
            write w a t
            read a
    stop $ r === pure t

prop_read_empty :: Key -> Property
prop_read_empty k = ioProperty $ do
  bucket' <- testBucket
  t <- runS3WithDefaults . read $ Address bucket' k
  pure $ t === Nothing

prop_getObjects_empty :: KeyTmp -> Property
prop_getObjects_empty p = ioProperty $ do
  bucket' <- testBucket
  objs <- runS3WithDefaults . getObjects $ Address bucket' (tmpPath p)
  pure $ fmap S3.objectKey objs === []

prop_getObjects :: Token -> KeyTmp -> Key -> Key -> Property
prop_getObjects token prefix p1 p2 = p1 /= p2 ==> ioProperty .
  runS3WithDefaults . withToken token $ \root -> do
    withTmpKey root $ prefix <//> p1
    withTmpKey root $ prefix <//> p2 <//> p1
    withTmpKey root $ prefix <//> p2 <//> p2
    objs <- getObjects root
    pure $ on (===) sort (fmap S3.objectKey objs) (fmap (unKey . (</>) (key root) . tmpPath) [prefix <//> p1, prefix <//> p2 <//> p1, prefix <//> p2 <//> p2])

prop_list :: Token -> Property
prop_list t = monadicIO $
  stop =<< (run . runS3WithDefaults . withToken t $ \a -> do
    write Fail a ""
    r' <- listRecursively (a { key = dirname $ key a })
    pure $ a `elem` r')

prop_dirname :: Address -> Text -> Property
prop_dirname a t =
  let k = Key t in
  T.all (/= '/') t ==>
    dirname (key a </> k) === key a


return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
