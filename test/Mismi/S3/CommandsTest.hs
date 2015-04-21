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

prop_exists :: Address -> Property
prop_exists x = monadicIO $ do
  r <- run $
    runS3WithDefaults . withAddress x $ \(Unique a) -> do
      write Fail a ""
      exists a
  stop $ r === True

prop_exists_empty :: Address -> Property
prop_exists_empty a = monadicIO $ do
  r <- run $
    runS3WithDefaults . withAddress a $ exists . runUnique
  stop $ r === False

prop_delete :: WriteMode -> Address -> Property
prop_delete w a' = monadicIO $ do
  r <- run $
    runS3WithDefaults . withAddress a' $ \(Unique a) -> do
      write w a ""
      x <- exists a
      delete a
      y <- exists a
      pure (x, y)
  stop $ r == (True, False)

prop_delete_empty :: Address -> Property
prop_delete_empty a' = monadicIO $ do
    r <- run $ (
      runS3WithDefaults . withAddress a' $ \(Unique a) -> do
        delete a
        pure True)
         `catchAll`
         (\(_) -> pure False)
    stop $ r === True

prop_read_write :: Address -> Text -> Property
prop_read_write a' d = monadicIO $ do
    r <- run $
      runS3WithDefaults . withAddress a' $ \(Unique a) -> do
        write Fail a d
        read a
    stop $ r === Just d

prop_write_download :: Address -> Text -> LocalPath -> Property
prop_write_download a' d l = monadicIO $ do
    r <- run $
      withSystemTempDirectory "mismi" $ \p ->
        runS3WithDefaults . withAddress a' $ \(Unique a) -> do
          write Fail a d
          let t = p F.</> localPath  l
          download a t
          liftIO $ T.readFile t
    stop $ r === d

prop_write_failure :: Address -> Text -> Property
prop_write_failure a' d = monadicIO $ do
    r <- run $ (
      runS3WithDefaults . withAddress a' $ \(Unique a) -> do
        write Fail a d
        write Fail a d
        pure False)
         `catchAll`
         (\(_) -> pure True)
    stop $ r === True

prop_write_overwrite :: Address -> UniquePair Text -> Property
prop_write_overwrite a' (UniquePair x y) = monadicIO $ do
    r <- run $
        runS3WithDefaults . withAddress a' $ \(Unique a) -> do
            write Fail a x
            write Overwrite a y
            read a
    stop $ r === pure y

-- |
-- If the object does not exist, then the behaviour should be invariant with the WriteMode
--
prop_write_nonexisting :: WriteMode -> Address -> Text -> Property
prop_write_nonexisting w a' t = monadicIO $ do
    r <- run $
        runS3WithDefaults . withAddress a' $ \(Unique a) -> do
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
  objs <- runS3WithDefaults . withTmpKey p $ getObjects . runUnique
  pure $ fmap S3.objectKey objs === []

prop_getObjects :: KeyTmp -> Key -> Key -> Property
prop_getObjects prefix p1 p2 = p1 /= p2 ==> ioProperty $
  runS3WithDefaults . withTmpKey (prefix <//> p1) $ \(Unique a) -> do
    let bucket' = bucket a
    let b = tmpBody prefix
    let p = dirname $ key a
    let address = Address $ bucket a
    write Fail (address $ p </> p2 </> p1) b
    write Fail (address $ p </> p2 </> p2) b
    objs <- getObjects $ Address bucket' p
    delete (address $ p </> p2 </> p2)
    delete (address $ p </> p2 </> p1)
    pure $ on (===) sort (fmap S3.objectKey objs) (fmap unKey [key a, p </> p2 </> p1, p </> p2 </> p2])

prop_list :: Address -> Property
prop_list a' = monadicIO $ do
  p <- run $
    runS3WithDefaults . withAddress a' $ \(Unique a) -> do
      write Fail a ""
      r <- listRecursively (a { key = dirname $ key a })
      return $ a `elem` r
  stop p

prop_dirname :: Address -> Text -> Property
prop_dirname a t =
  let k = Key t in
  T.all (/= '/') t ==>
    dirname (key a </> k) === key a


return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
