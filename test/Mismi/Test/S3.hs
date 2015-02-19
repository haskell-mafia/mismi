{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mismi.Test.S3 where

import qualified Aws.S3 as S3

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Catch (bracket_)

import           Data.Text as T

import           Network.HTTP.Client (RequestBody(..))

import           System.Posix.Env

import           Mismi.Arbitrary ()
import           Mismi.Control
import           Mismi.S3.Control
import           Mismi.S3.Data
import           Mismi.Test




testBucket :: IO Bucket
testBucket =
  Bucket . T.pack . fromMaybe "ambiata-dev-view" <$> getEnv "AWS_TEST_BUCKET"

-- Ensure everything is under our own key space for debugging
tmpPath :: Key -> Key
tmpPath path' = (Key "tmp/vee") </> path'

withTmpKey :: Testable t => Key -> (Key -> S3Action t) -> S3Action t
withTmpKey path' f = do
  (Bucket bucket') <- liftIO $ testBucket
  let tmp@(Key tmpPath') = tmpPath path'
  bracket_
    (awsRequest (S3.putObject bucket' tmpPath' (RequestBodyBS "")))
    (awsRequest $ S3.DeleteObject tmpPath' bucket')
    (f tmp)
