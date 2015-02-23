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
  T.count "//" (showKey (p1 </> p2)) === 0


prop_parseShowKeyWeakInverse :: Key -> Property
prop_parseShowKeyWeakInverse p = (parseKey . showKey) p === p

return []
tests :: IO Bool
tests = $quickCheckAll
