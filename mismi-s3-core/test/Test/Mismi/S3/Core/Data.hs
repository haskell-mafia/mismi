{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Mismi.S3.Core.Data where

import           Data.Text as T
import           Data.List as L (sort)
import           Disorder.Corpus

import           Mismi.S3.Core.Data

import           P

import           System.IO

import           Test.QuickCheck
import           Test.Mismi.S3.Core.Arbitrary ()


prop_append :: Key -> Key -> Property
prop_append p1 p2 =
  T.count "//" (unKey (p1 // p2)) === 0

prop_appendEdge :: Property
prop_appendEdge = forAll ((,) <$> elements muppets <*> elements southpark) $ \(m, s) -> conjoin [
    (Key (m <> "/") // Key s) === (Key $ m <> "/" <> s)
  , (Key m // Key ("/" <> s)) === (Key $ m <> "/" <> s)
  , (Key m // Key s) === (Key $ m <> "/" <> s)
  ]

prop_parse :: Address -> Property
prop_parse a =
  addressFromText (addressToText a) === Just a

prop_parse_bucket :: Bucket -> Property
prop_parse_bucket b =
  addressFromText ("s3://" <> unBucket b) === Just (Address b (Key ""))

prop_sorted :: [Address] -> Property
prop_sorted addresses =
  fmap addressToText (L.sort addresses) === L.sort (fmap addressToText addresses)

prop_withKey :: Address -> Property
prop_withKey a =
  withKey id a === a

prop_withKey_dirname :: Address -> Property
prop_withKey_dirname a =
  key (withKey dirname a) === (dirname . key) a

prop_withKey_key :: Address -> Key -> Property
prop_withKey_key a k =
  key (withKey (// k) a) === (key a) // k

prop_basename :: Key -> Text -> Property
prop_basename k bn = T.all (/= '/') bn && not (T.null bn) ==>
  basename (k // (Key bn)) === Just bn

prop_basename_prefix :: Key -> Text -> Property
prop_basename_prefix k bn =
  basename (k // (Key $ bn <> "/")) === Nothing

prop_basename_root :: Property
prop_basename_root =
  basename (Key "") === Nothing

prop_dirname :: Address -> Text -> Property
prop_dirname a t =
  let k = Key t in
  T.all (/= '/') t==>
    dirname (key a // k) === Key (dropWhileEnd ('/' ==) . unKey . key $ a)

prop_commonPrefix :: Address -> Key -> Property
prop_commonPrefix a k =
  removeCommonPrefix a (withKey (// k) a) === Just k

prop_commonPrefix_fail :: Address -> Key -> Property
prop_commonPrefix_fail a k =
  removeCommonPrefix (withKey (// k) a) a === Nothing


return []
tests :: IO Bool
tests = $quickCheckAll
