{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Mismi.SQS.Aws.Commands where

import           Control.Lens
import           Data.Maybe
import           Disorder.Core.IO

import qualified Mismi.SQS as A
import           Mismi.SQS.Aws

import           P

import           System.IO

import           Test.Mismi.SQS.Aws
import           Test.QuickCheck


prop_read_compatibility :: Queue -> NonEmptyMessage -> Property
prop_read_compatibility queue' (NonEmptyMessage b) =
  testIO . runSQSWithQueueArg (Just 0) queue' $ \q -> do
    void $ writeMessage q b Nothing
    ms  <- readMessages q (Just 1) Nothing
    ms' <- A.readMessages q (Just 1) Nothing
    pure $
      [b] === (mBody <$> ms) .&&.
      [Just b] === ((^. A.mBody) <$> ms') .&&.
      let ([m], [m']) = (ms, ms')
      in
        Just (mMessageId m) === m' ^. A.mMessageId .&&.
        Just (mMD5OfBody m) === m' ^. A.mMD5OfBody .&&.
        mMD5OfMessageAttributes m === m' ^. A.mMD5OfMessageAttributes


return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 2 })
