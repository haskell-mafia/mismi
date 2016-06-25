{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Mismi.SQS.Data (
    QueueName(..)
  , Queue(..)
  , QueueUrl(..)
  , MessageId(..)
  , SQSError(..)
  , sqsErrorRender
  , module Mismi.SQS.Core.Data
  ) where

import           Control.Exception.Base
import           Data.Text
import           Data.Typeable
import           Mismi.SQS.Core.Data
import           P

data SQSError =
    Invariant Text
    deriving (Typeable)

instance Exception SQSError

instance Show SQSError where
  show = unpack . sqsErrorRender

sqsErrorRender :: SQSError -> Text
sqsErrorRender (Invariant e) =
  "[Mismi internal error] - " <> e
