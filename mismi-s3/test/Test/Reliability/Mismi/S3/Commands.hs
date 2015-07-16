{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Reliability.Mismi.S3.Commands where

import           Control.Monad.IO.Class
import           Control.Monad.Catch

import           Data.Text (Text)
import qualified Data.Text as T

import           Disorder.Corpus
import           Disorder.Core.IO

import           Mismi.S3.Control
import           Mismi.S3.Commands
import           Mismi.S3.Data

import           P
import qualified Prelude as P

import           System.IO
import           System.IO.Error
import           System.Environment

import           Test.Mismi.S3
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

prop_list :: Property
prop_list = forAll (elements muppets) $ \m -> testS3 $ \a i -> do
  createFiles a m i
  replicateM_ 100 (list a >>= \z -> when (length z /=  i) (throwM $ userError "List is not the same as original response"))
  pure $ True === True

createFiles :: Address -> Text -> Int -> S3Action ()
createFiles prefix name n = do
  mapM_ (\i ->
          flip write "data" $
            withKey (</> Key (name <> "-" <> (T.pack $ show i))) prefix
        ) [1..n]

testS3 :: Testable a => (Address -> Int -> S3Action a) -> Property
testS3 f =
  property $ \t -> testIO .
    retryHttpWithMessage 5 "Reliability" . runS3WithDefaults . withToken t $ \a -> do
      i <- liftIO $ testSize
      f a i

testSize :: IO Int
testSize = do
  view <- lookupEnv "TEST_RELIABILITY_SIZE"
  let size = maybe 10 P.read view
  pure size

getMaxSuccess :: IO Int
getMaxSuccess = do
  view <- lookupEnv "TEST_RELIABILITY_SUCCESS"
  let size = maybe 5 P.read view
  pure size

return []
tests :: IO Bool
tests =
  getMaxSuccess >>= testsN

testsN :: Int -> IO Bool
testsN n =
  $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = n })
