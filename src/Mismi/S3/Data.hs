{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Mismi.S3.Data (
    Bucket(..)
  , Address (..)
  , Key (..)
  , (</>)
  ) where

import           Data.Text as T

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
  show (Address (Bucket b) (Key k)) =
    T.unpack $ "s3://" <> b <> "/" <> k

(</>) :: Key -> Key -> Key
(</>) (Key p1) (Key p2) = Key $ p1 <> "/" <> p2
