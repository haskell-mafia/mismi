{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mismi.DynamoDB.Control (
    configureRetries
  ) where

import           Control.Lens (over, set, (^.), has)

import           Mismi (Env)
import           Mismi.Amazonka (serviceRetry, retryAttempts, exponentBase, configure, retryCheck, hasCode, hasStatus)

import           Network.AWS.DynamoDB (dynamoDB)

import           P

-- https://github.com/brendanhay/amazonka/issues/351
configureRetries :: Env -> Env
configureRetries env =
  let
    check e =
      let
        check' = dynamoDB ^. serviceRetry . retryCheck
      in
        case check' e of
          Nothing ->
            case has (hasCode "ProvisionedThroughputExceeded" . hasStatus 400) e of
              True ->
                Just "throughput_exceeded_x"
              False ->
                Nothing
          Just x ->
            Just x
  in
    flip configure env .
      flip (over serviceRetry) dynamoDB $
          set retryAttempts 7 .
          set exponentBase 0.6 .
          set retryCheck check
