{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Mismi.S3 (
    module X
  , Token (..)
  , LocalPath (..)
  , testBucket
  , withToken
  ) where

import           Control.Monad.IO.Class (liftIO)

import qualified Data.List as L
import           Data.Text as T
import           Data.UUID as U
import           Data.UUID.V4 as U

import           Disorder.Corpus

import           System.Posix.Env
import           System.FilePath hiding ((</>))

import           Mismi.Control
import           Mismi.S3

import           P

import           System.IO

import           Test.QuickCheck
import           Test.Mismi as X
import           Test.Mismi.Arbitrary ()
import           Test.Mismi.S3.Arbitrary ()

data Token =
  Token {
      unToken :: Text
    } deriving (Eq, Show)

instance Arbitrary Token where
  arbitrary = do
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

withToken :: Token -> (Address -> AWS a) -> AWS a
withToken t f = do
  b <- liftIO testBucket
  u <- liftIO $ T.pack . U.toString <$> U.nextRandom
  let a = Address b (Key . T.intercalate "/" $ ["mismi", u, unToken t])
  awsBracket_ (pure ()) (listRecursively a >>= mapM_ delete >> delete a) (f a)
