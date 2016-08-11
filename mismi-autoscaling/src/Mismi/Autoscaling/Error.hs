{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Mismi.Autoscaling.Error (
    AutoScalingError (..)
  , retry
  , continueIfExists
  , catchValidationError
  , scalingInProgress
  , resourceInUse
  , alreadyExists
  , throttling
  , validationError
  ) where

import           Control.Lens ((^.))
import           Control.Monad.Catch (Handler (..))
import           Control.Retry (RetryPolicyM, RetryStatus, recovering)

import           Mismi.Amazonka (AWS, Error (..), ErrorCode, errorCode, serviceCode, serviceStatus)
import           Mismi.Control (handleServiceError)

import           Network.HTTP.Types.Status (status400)

import           P

retry :: RetryPolicyM AWS -> [AutoScalingError] -> AWS a -> AWS a
retry r ecs action =
  recovering r (serviceCodeHandler ecs) $ const action

continueIfExists :: AWS () -> AWS ()
continueIfExists =
  handleServiceError (\se ->
    (se ^. serviceStatus == status400 && se ^. serviceCode == autoScalingErrorCode alreadyExists)) ()

catchValidationError :: a -> AWS a -> AWS a
catchValidationError f =
  handleServiceError (\se -> (se ^. serviceStatus == status400 && se ^. serviceCode == autoScalingErrorCode validationError)) f

newtype AutoScalingError =
  AutoScalingError {
      renderAutoScalingError :: Text
    } deriving (Eq, Show)

autoScalingErrorCode :: AutoScalingError -> ErrorCode
autoScalingErrorCode =
  errorCode . renderAutoScalingError

scalingInProgress :: AutoScalingError
scalingInProgress =
  AutoScalingError "ScalingActivityInProgress"

validationError :: AutoScalingError
validationError =
  AutoScalingError "ValidationError"

resourceInUse :: AutoScalingError
resourceInUse =
  AutoScalingError "ResourceInUse"

alreadyExists :: AutoScalingError
alreadyExists =
  AutoScalingError "AlreadyExists"

throttling :: AutoScalingError
throttling =
  AutoScalingError "Throttling"

serviceCodeHandler :: [AutoScalingError] -> [RetryStatus -> Handler AWS Bool]
serviceCodeHandler ecs = [
  const $ Handler $ \(e :: Error) ->
    case e of
      ServiceError se ->
        pure . or $ (se ^. serviceCode == autoScalingErrorCode throttling)
         : fmap ((==) (se ^. serviceCode) . autoScalingErrorCode) ecs
      SerializeError _ ->
        pure False
      TransportError _ ->
        pure False
  ]
