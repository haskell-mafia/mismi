{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Mismi.S3.Data (
    Bucket(..)
  , Address (..)
  , Key (..)
  , (</>)
  , dirname
  ) where

import qualified Data.Text as T
import           Data.Text (Text)
import           Data.List (init)

import           P


newtype Bucket = Bucket {
    unBucket :: Text
  } deriving (Eq, Show)

data Address = Address {
    bucket :: Bucket
  , key :: Key
  } deriving Eq

-- NOTE: This is not a "safe" data type, and makes no guarantee about what is _actually_ supported for S3
-- https://github.com/ambiata/mismi/issues/2
newtype Key = Key {
    unKey :: Text
  } deriving (Eq, Show)

instance Show Address where
  show (Address b k) =
    "Address (" <> show b <> ") (" <> show k <> ")"

(</>) :: Key -> Key -> Key
(</>) (Key p1) (Key p2) = Key $ p1 <> "/" <> p2

dirname :: Key -> Key
dirname =
  Key . T.intercalate "/" . init . T.split (=='/') . unKey
