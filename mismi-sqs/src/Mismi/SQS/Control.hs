{-# LANGUAGE NoImplicitPrelude #-}
module Mismi.SQS.Control (
    module X
  , runSQSWithDefaults
  , runSQSWithRegion
  ) where

import           Mismi.Control as X
import           System.IO

runSQSWithDefaults :: AWS b -> IO b
runSQSWithDefaults = runAWSDefaultRegion

runSQSWithRegion :: Region -> AWS a -> IO a
runSQSWithRegion r = runAWSWithRegion r
