{-# LANGUAGE NoImplicitPrelude  #-}
module Mismi.IAM.Data (
    AccountNumber(..)
  ) where

import           Mismi.Amazonka

import           P


newtype AccountNumber = AccountNumber {
    unAccountNumber :: Text
  } deriving (Eq, Show)

instance ToText AccountNumber where
  toText = unAccountNumber

instance FromText AccountNumber where
  parser = AccountNumber <$> parser
