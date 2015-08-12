{-# LANGUAGE NoImplicitPrelude #-}
module Mismi.SQS.Aws.Control (
    SQSAction
  , runSQSWithDefaults
  , runSQSWithRegion
  , liftSQSAction
  ) where

import qualified Mismi.SQS.Control as A
import           Mismi.SQS.Control hiding (runSQSWithDefaults, runSQSWithRegion)
import           P
import           System.IO

-- | Specilised AwsAction for SQS operations
type SQSAction = AWS

liftSQSAction :: SQSAction a -> A.AWS a
liftSQSAction = id

runSQSWithDefaults :: SQSAction b -> IO b
runSQSWithDefaults = A.runSQSWithDefaults

runSQSWithRegion :: Region -> SQSAction a -> IO a
runSQSWithRegion = A.runSQSWithRegion
