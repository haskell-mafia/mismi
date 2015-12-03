{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Mismi.S3 (
    module X
  , Token (..)
  , LocalPath (..)
  , testBucket
  , createSmallFiles
  , files
  , newAddress
  , newFilePath
  , addCleanupFinalizer
  , addPrintFinalizer
  , addLocalCleanupFinalizer
  , addLocalPrintFinalizer
  ) where


import           Control.Monad.Catch
import           Control.Monad.Reader (ask)
import           Control.Monad.Trans.Resource
import           Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.List as L
import           Data.Text as T
import qualified Data.Text.IO as T
import           Data.UUID as U
import           Data.UUID.V4 as U

import           Disorder.Corpus

import           System.Environment (lookupEnv)
import           System.Posix.Env
import           System.FilePath
import           System.Directory

import           Mismi.Control
import           Mismi.S3

import           P

import           System.IO

import           Test.QuickCheck
import           Test.Mismi as X
import           Test.Mismi.Arbitrary ()
import           Test.Mismi.S3.Arbitrary ()

import           X.Control.Monad.Trans.Either

data Token =
  Token {
      unToken :: Text
    } deriving (Eq, Show)

instance Arbitrary Token where
  arbitrary =
    genToken

genToken :: Gen Token
genToken = do
  n <- T.pack . show <$> (choose (0, 10000) :: Gen Int)
  c <- elements cooking
  m <- elements muppets
  sep <- elements ["-", "=", "."]
  pure . Token . T.intercalate sep $ [c, m, n]

data LocalPath =
  LocalPath {
      localPath :: FilePath
    } deriving (Eq, Show)

instance Arbitrary LocalPath where
  arbitrary = do
    x <- elements weather
    xs <- listOf $ elements simpsons
    pure . LocalPath $ L.intercalate "/" (T.unpack <$> x : xs)

testBucket :: IO Bucket
testBucket =
  Bucket . T.pack . fromMaybe "ambiata-dev-view" <$> getEnv "AWS_TEST_BUCKET"

createSmallFiles :: Address -> Text -> Int -> AWS ()
createSmallFiles prefix name n = do
  mapM_ (flip write "data") $ files prefix name n

files :: Address -> Text -> Int -> [Address]
files prefix name n =
  fmap (\i -> withKey (// Key (name <> "-" <> (T.pack $ show i))) prefix) [1..n]

newAddress :: AWS Address
newAddress = do
  a <- liftIO $ do
    t <- generate genToken
    b <- testBucket
    u <- T.pack . U.toString <$> U.nextRandom
    pure $ Address b (Key . T.intercalate "/" $ ["mismi", u, unToken t])
  addCleanupFinalizer a
  addPrintFinalizer a
  pure $ a

newFilePath :: AWS FilePath
newFilePath = do
  p <- liftIO $ do
    t <- generate genToken
    d <- getTemporaryDirectory
    u <- liftIO $ U.toString <$> U.nextRandom
    let p = d <> "/mismi/" <> u <> "-" <> (T.unpack . unToken $ t)
    createDirectoryIfMissing True p
    pure p
  addLocalCleanupFinalizer p
  addLocalPrintFinalizer p
  pure p

vk :: MonadIO m => Text -> m Bool
vk k = do
  m <- liftIO $ lookupEnv (T.unpack k)
  return $ maybe False (\v -> v == "1" || v == "true") m

addCleanupFinalizer :: Address -> AWS ()
addCleanupFinalizer a = do
  e <- ask
  unlessM (vk "TEST_SKIP_CLEANUP_RESOURCES") $ do
    void $ register (eitherT throwM pure . runAWS e $ listRecursively a >>= mapM_ delete >> delete a)
    void $ register (T.putStrLn $ "Cleaning up [" <> addressToText a <> "]")

addPrintFinalizer :: Address -> AWS ()
addPrintFinalizer a =
  whenM (vk "TEST_PRINT_PATHS") .
    void $ register (T.putStrLn $ "Temporary s3 address [" <> addressToText a <> "]")

addLocalCleanupFinalizer :: FilePath -> AWS ()
addLocalCleanupFinalizer a = do
  unlessM (vk "TEST_SKIP_CLEANUP_RESOURCES") $ do
    void $ register (removeDirectoryRecursive a)
    void $ register (T.putStrLn $ "Cleaning up [" <> T.pack a <> "]")

addLocalPrintFinalizer :: FilePath -> AWS ()
addLocalPrintFinalizer a =
  whenM (vk "TEST_PRINT_PATHS") .
    void $ register (T.putStrLn $ "Temporary local filepath [" <> T.pack a <> "]")
