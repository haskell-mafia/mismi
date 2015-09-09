{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Mismi.Control where

import           Control.Monad.Catch hiding (finally)
import           Control.Monad.IO.Class

import           Data.IORef
import           Data.Text

import           Disorder.Core.IO

import           Mismi.Control

import           P


import           System.IO
import           System.IO.Error (userError)

import           Test.Mismi
import           Test.Mismi.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


prop_bracket :: [Text] -> Text -> Text -> Property
prop_bracket l final action = final /= "" && action /= "" ==> testIO $ do
  r <- newIORef l
  let after' = (flip modifyIORef (final :))
  let action' = (flip modifyIORef (action :))
  runAWSDefaultRegion $ awsBracket (liftIO $ return r) (liftIO . after') (liftIO . action')
  (=== final : action : l) <$> readIORef r

prop_bracket_catch :: [Text] -> Text -> Property
prop_bracket_catch l final = final /= "" ==> testIO $ do
  r <- newIORef l
  let after' = (flip modifyIORef (final :))
  let action' = const $ throwM (userError "")
  runAWSDefaultRegion $ awsBracket (liftIO $ return r) (liftIO . after') (liftIO . action') `catchIOError` (const $ return ())
  (=== final : l) <$> readIORef r


return []
tests :: IO Bool
tests = $quickCheckAll
