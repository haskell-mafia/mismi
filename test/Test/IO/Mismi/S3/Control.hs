{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Mismi.S3.Control where

import           Data.Text

import           Mismi.S3.Control
import           Mismi.S3.Commands

import           Test.Mismi
import           Test.Mismi.S3
import           Test.Mismi.S3 ()

prop_liftS3 :: Token -> Text -> Property
prop_liftS3 t d = testAWS Sydney $ do
  r' <- liftS3Action (withToken t $ \a -> do
    write a d
    read a)
  pure $ r' === Just d

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 10 })
