{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Mismi.S3.DataTest where

import           Data.Text as T

import           Mismi.S3.Data
import           Mismi.Test
import           Mismi.Test.S3 ()


prop_append :: Key -> Key -> Property
prop_append p1 p2 =
  T.count "//" (unKey (p1 </> p2)) === 0


return []
tests :: IO Bool
tests = $quickCheckAll
