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

prop_parse :: Address -> Property
prop_parse a =
  addressFromText (addressToText a) === Just a

prop_withKey :: Address -> Property
prop_withKey a =
  withKey id a === a

prop_withKey_dirname :: Address -> Property
prop_withKey_dirname a =
  key (withKey dirname a) === (dirname . key) a

prop_withKey_key :: Address -> Key -> Property
prop_withKey_key a k =
  key (withKey (</> k) a) === (key a) </> k

return []
tests :: IO Bool
tests = $quickCheckAll
