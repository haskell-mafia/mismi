{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mismi.Test.S3 where

import qualified Aws.S3 as S3

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Catch (bracket_, finally)

import qualified Data.List as L
import           Data.Text as T
import           Data.Text.Encoding as T
import           Data.UUID ( toString )
import           Data.UUID.V4 ( nextRandom )

import           Network.HTTP.Client (RequestBody(..))

import           System.Posix.Env
import           System.FilePath hiding ((</>))

import           Mismi.Arbitrary ()
import           Mismi.Control
import           Mismi.S3.Control
import           Mismi.S3.Data
import           Mismi.Test

import           Orphanarium.Corpus


newtype Unique a = Unique a deriving (Show, Eq)

runUnique :: Unique a -> a
runUnique (Unique x) = x

instance Functor Unique where
    fmap f (Unique x) = Unique $ f x

makeUniqueKey :: Key -> IO (Unique Key)
makeUniqueKey k = do
    u <- nextRandom
    let ut = T.pack $ toString u
    return . Unique $ Key "tests" </> Key ut </> k

data KeyTmp = KeyTmp {
    tmpPath' :: Key
  , tmpBody :: Text
  } deriving (Eq, Show)

tmpPath :: KeyTmp -> Key
tmpPath = (Key "tmp/vee" </>) . tmpPath'

makeUniqueKeyTmp :: KeyTmp -> IO (Unique KeyTmp)
makeUniqueKeyTmp (KeyTmp k b) = fmap (flip KeyTmp b) <$> makeUniqueKey k

-- Ensure everything is under our own key space for debugging
instance Arbitrary KeyTmp where
  arbitrary = KeyTmp  <$> arbitrary <*> arbitrary

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

withTmpKey :: KeyTmp -> (Unique Address -> S3Action t) -> S3Action t
withTmpKey kt f = do
  (Bucket bucket') <- liftIO testBucket
  u <- liftIO $ makeUniqueKeyTmp kt
  let uniqueTmpPath = tmpPath . runUnique $ u
  let body' = tmpBody . runUnique $ u
  bracket_
    (awsRequest (S3.putObject bucket' (unKey uniqueTmpPath) (RequestBodyBS (T.encodeUtf8 body'))))
    (awsRequest $ S3.DeleteObject (unKey uniqueTmpPath) bucket')
    (f . Unique $ Address (Bucket bucket') uniqueTmpPath)

withKey :: Key -> (Unique Address -> S3Action t) -> S3Action t
withKey k f = do
    bucket' <- liftIO testBucket
    withAddress (Address bucket' k) f

withAddress :: Address -> (Unique Address -> S3Action t) -> S3Action t
withAddress a f = do
    u <- liftIO . makeUniqueKey $ key a
    let uniqueAddress = fmap (Address (bucket a)) u
    finally (f uniqueAddress) (awsRequest $ S3.DeleteObject (unKey . runUnique $ u) (unBucket . bucket $ a))
