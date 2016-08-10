{-# LANGUAGE NoImplicitPrelude  #-}
module Mismi.Autoscaling.Data ( -- Config maybe?
    continueIfExists
  , scalingInProgress
  , resourceInUse
  , alreadyExists
  , handleServiceError
  , retry
  , throttling
  , minimumDesiredInstances
  , groupTags
  ) where

import           Control.Lens ((^.), (.~), view)
import           Control.Monad.Catch (Handler (..), throwM, catch)
import           Control.Retry (RetryPolicyM, RetryStatus, recovering)

import           Mismi.Amazonka (AWS, Error (..), ServiceError (..), errorCode, serviceCode, serviceStatus)
import           Mismi.Autoscaling.Core.Data
import qualified Mismi.Autoscaling.Amazonka as A

import           Network.HTTP.Types.Status (status400)

import           P

groupTags :: GroupName -> [GroupTag] -> [A.Tag]
groupTags (GroupName name) =
  fmap (\(GroupTag (EC2Tag k v) p) ->
    A.tag k name "auto-scaling-group" (propagateToBool p) v)


minimumDesiredInstances :: DesiredInstances -> DesiredInstances
minimumDesiredInstances (DesiredInstances i) =
  DesiredInstances $ bool i 1 (i > 1)

retry :: RetryPolicyM AWS -> [Text] -> AWS a -> AWS a
retry r ecs action =
  recovering r (serviceCodeHandler ecs) $ const action

continueIfExists :: AWS () -> AWS ()
continueIfExists =
  handleServiceError (\se -> (se ^. serviceStatus == status400 && se ^. serviceCode == errorCode alreadyExists)) ()

scalingInProgress :: Text
scalingInProgress =
  "ScalingActivityInProgress"

resourceInUse :: Text
resourceInUse =
  "ResourceInUse"

alreadyExists :: Text
alreadyExists =
  "AlreadyExists"

throttling :: Text
throttling =
  "Throttling"

handleServiceError :: (ServiceError -> Bool) -> a -> AWS a -> AWS a
handleServiceError f pass action =
  action `catch` \(e :: Error) ->
    case e of
      ServiceError se ->
        if f se
          then pure pass
          else throwM e
      SerializeError _ ->
        throwM e
      TransportError _ ->
        throwM e

serviceCodeHandler :: [Text] -> [RetryStatus -> Handler AWS Bool]
serviceCodeHandler ecs = [
  const $ Handler $ \(e :: Error) ->
    case e of
      ServiceError se ->
        pure . or $ (se ^. serviceCode == errorCode throttling)
         : fmap ((==) (se ^. serviceCode) . errorCode) ecs
      SerializeError _ ->
        pure False
      TransportError _ ->
        pure False
  ]
