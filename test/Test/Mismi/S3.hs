{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Mismi.S3 (
    Token
  , LocalPath (..)
  , testBucket
  , withToken
  , testAWS
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Catch (bracket_)
import           Control.Monad.Trans.Either

import qualified Data.List as L
import           Data.Text as T
import           Data.UUID as U
import           Data.UUID.V4 as U

import           Disorder.Corpus
import           Disorder.Core
import           Disorder.Core.IO

import           Control.Monad.Trans.AWS hiding (getEnv)

import           System.Posix.Env
import           System.FilePath hiding ((</>))

import           Mismi.Control
import           Mismi.S3.Control
import           Mismi.S3.Commands
import           Mismi.S3.Data

import           Test.Mismi
import           Test.Mismi.Arbitrary ()

data Token =
  Token {
      unToken :: Text
    } deriving (Eq, Show)

instance Arbitrary Token where
  arbitrary = do
    n <- T.pack . show <$> (choose (0, 10000) :: Gen Int)
    c <- elements cooking
    m <- elements muppets
    pure . Token . T.intercalate "." $ [c, m, n]

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

withToken :: Token -> (Address -> S3Action a) -> S3Action a
withToken t f = do
  b <- liftIO testBucket
  u <- liftIO $ T.pack . U.toString <$> U.nextRandom
  let a = Address b (Key . T.intercalate "/" $ ["mismi", u, unToken t])
  bracket_ (pure ()) (listRecursively a >>= mapM_ delete >> delete a) (f a)

testAWS :: Testable a => Region -> AWS a -> Property
testAWS r a =
  testIO $ do
    e <- runEitherT $ runAWS r a
    pure $ either (\e' -> failWith $ "Property failed [" <> awsErrorRender e' <> "].") property e
