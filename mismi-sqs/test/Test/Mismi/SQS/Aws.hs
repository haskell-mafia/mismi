{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Mismi.SQS.Aws (
    module A
  , withQueue
  , runSQSWithCfgWithDefaults
  , runSQSWithQueue
  ) where

import           Mismi.SQS.Aws

import qualified Test.Mismi.SQS as A
import           Test.Mismi.SQS hiding (withQueue, runSQSWithQueue, runSQSWithCfgWithDefaults)

import           System.IO


withQueue :: QueueName -> (QueueUrl -> SQSAction a) -> SQSAction a
withQueue = A.withQueue

runSQSWithQueue :: Queue -> (QueueUrl -> SQSAction a) -> IO a
runSQSWithQueue= A.runSQSWithQueue

runSQSWithCfgWithDefaults :: SQSAction b -> IO b
runSQSWithCfgWithDefaults = A.runSQSWithCfgWithDefaults
