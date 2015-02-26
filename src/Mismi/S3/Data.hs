{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Mismi.S3.Data (
    Bucket(..)
  , Address (..)
  , Key (..)
  , (</>)
  , parseKey
  , showAddress
  , showKey
  ) where

import           Mismi.S3.Data.Component

import qualified Data.Text as T

import           P


newtype Bucket = Bucket
    { unBucket :: Component
    } deriving (Eq, Show)

data Address = Address
    { bucket  :: Bucket
    , key     :: Key
    } deriving (Eq, Show)

-- NOTE: This is not a "safe" data type, and makes no guarantee about what is _actually_ supported for S3
-- https://github.com/ambiata/mismi/issues/2
infixl 5 :/
data Key =
        EmptyKey
    |   Key :/ Component

showKey :: Key -> T.Text
showKey EmptyKey = ""
showKey (k :/ c) = T.intercalate "/" [showKey k, componentText c]

-- |
-- Right now this doesnt check for and reject invalid characters,
-- We need to wrap `Key` around something else
--
parseKey :: T.Text -> Key
parseKey = foldl' (</>) EmptyKey . parseDelimitedText

showAddress :: Address -> T.Text
showAddress (Address (Bucket b) k) = "s3://" <> componentText b <> "/" <> showKey k

infixl 5 </>
(</>) :: Key -> Component -> Key
(</>) = (:/)
