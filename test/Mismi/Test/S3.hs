{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Mismi.Test.S3 where

import qualified Aws.S3 as S3

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Catch (bracket_)

import           Data.Text as T
import           Data.Text.Encoding as T

import           Network.HTTP.Client (RequestBody(..))

import           System.Posix.Env

import           Mismi.Arbitrary ()
import           Mismi.Control
import           Mismi.S3.Control
import           Mismi.S3.Data
import           Mismi.S3.Data.Component
import           Mismi.S3.Data.Component.QQ

import           Mismi.Test.S3.Data.Component
import           Mismi.Test


data KeyTmp = KeyTmp {
    tmpPath :: Key
  , tmpBody :: Text
  } deriving (Eq, Show)


-- Ensure everything is under our own key space for debugging
instance Arbitrary KeyTmp where
  arbitrary = KeyTmp  <$> (([qcomponent|tmp|] <:/> [qcomponent|vee|] </>) <$> arbitrary) <*> arbitrary


(<//>) :: KeyTmp -> Component -> KeyTmp
(<//>) (KeyTmp k1 b) k2 = KeyTmp (k1 </> k2) b

testBucket :: IO Bucket
testBucket =
  Bucket . T.pack . fromMaybe "ambiata-dev-view" <$> getEnv "AWS_TEST_BUCKET"

withTmpKey :: KeyTmp -> S3Action t -> S3Action t
withTmpKey (KeyTmp tmpPath' body') f = do
  (Bucket bucket') <- liftIO testBucket
  bracket_
    (awsRequest (S3.putObject bucket' (showKey tmpPath') (RequestBodyBS (T.encodeUtf8 body'))))
    (awsRequest $ S3.DeleteObject (showKey tmpPath') bucket')
    f
