{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Mismi.Control where

import           Control.Monad.Catch hiding (finally)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource

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

prop_testAWS :: Property
prop_testAWS =
  expectFailure . Test.QuickCheck.once . testAWS $ pure False


prop_finalizer = testIO $ do
  r <- newIORef (0 :: Int)
  runAWSDefaultRegion $ do
    void $ register (modifyIORef r (const $ 1))
  (=== 1) <$> readIORef r




return []
tests :: IO Bool
tests = $quickCheckAll
