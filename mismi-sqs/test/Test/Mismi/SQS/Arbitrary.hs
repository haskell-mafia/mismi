{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Mismi.SQS.Arbitrary where

import           Test.Mismi.Arbitrary ()
import           Test.Mismi.SQS.Core.Arbitrary ()
import           Test.QuickCheck ()
import           Test.QuickCheck.Instances ()
