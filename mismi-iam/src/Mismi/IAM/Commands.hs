{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mismi.IAM.Commands (
    lookupCurrentAccountNumber
  ) where

import           Control.Lens (view)

import           Data.Text (splitOn)

import           Mismi.Amazonka
import           Mismi.IAM.Amazonka
import           Mismi.IAM.Data

import           P

-- |
-- Extract the current user's "AWS account ID"
-- Permissions required: 'iam:GetUser'
lookupCurrentAccountNumber :: AWS (Maybe AccountNumber)
lookupCurrentAccountNumber = do
  r <- send getUser
  return . extractAccountNumber . view (gursUser . uARN) $ r


extractAccountNumber :: Text -> Maybe AccountNumber
extractAccountNumber =
  -- arn:partition:service:region:account-id:resource
  fmap AccountNumber . listToMaybe . drop 4 . splitOn ":"
