{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mismi.Test.S3 where

import qualified Aws.S3 as S3

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Catch (bracket_, finally)

import qualified Data.List as L
import           Data.Text as T
import           Data.Text.Encoding as T

import           Network.HTTP.Client (RequestBody(..))

import           System.Posix.Env
import           System.FilePath hiding ((</>))

import           Mismi.Arbitrary ()
import           Mismi.Control
import           Mismi.S3.Control
import           Mismi.S3.Data
import           Mismi.Test

import           Orphanarium.Corpus


data KeyTmp = KeyTmp {
    tmpPath :: Key
  , tmpBody :: Text
  } deriving (Eq, Show)


-- Ensure everything is under our own key space for debugging
instance Arbitrary KeyTmp where
  arbitrary = KeyTmp  <$> ((Key "tmp/vee" </>) <$> arbitrary) <*> arbitrary

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

(<//>) :: KeyTmp -> Key -> KeyTmp
(<//>) (KeyTmp k1 b) k2 = KeyTmp (k1 </> k2) b

withTmpKey :: KeyTmp -> (S3Action t) -> S3Action t
withTmpKey (KeyTmp (Key tmpPath') body') f = do
  (Bucket bucket') <- liftIO $ testBucket
  bracket_
    (awsRequest (S3.putObject bucket' tmpPath' (RequestBodyBS (T.encodeUtf8 body'))))
    (awsRequest $ S3.DeleteObject tmpPath' bucket')
    f

withKey :: Key -> (S3Action t) -> S3Action t
withKey k f = do
  (Bucket bucket') <- liftIO $ testBucket
  finally f (awsRequest $ S3.DeleteObject (unKey k) bucket')

withAddress :: Address -> (S3Action t) -> S3Action t
withAddress a f = do
  finally f (awsRequest $ S3.DeleteObject (unKey $ key a) (unBucket $ bucket a))
