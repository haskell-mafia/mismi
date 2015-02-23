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

import           Data.Text as T

import           P


newtype Bucket = Bucket
    { unBucket :: Text
    } deriving (Eq, Show)

data Address = Address
    { bucket  :: Bucket
    , key     :: Key
    } deriving (Eq, Show)

-- NOTE: This is not a "safe" data type, and makes no guarantee about what is _actually_ supported for S3
-- https://github.com/ambiata/mismi/issues/2
newtype Key = Key
    { unKey :: [Text]
    } deriving (Eq, Show)

showKey :: Key -> T.Text
showKey (Key ps) = T.intercalate "/" ps

-- |
-- Right now this doesnt check for and reject invalid characters,
-- We need to wrap `Key` around something else
--
parseKey :: T.Text -> Key
parseKey = Key . P.filter (not . T.null) . T.splitOn "/"

showAddress :: Address -> T.Text
showAddress (Address (Bucket b) k) = "s3://" <> b <> "/" <> showKey k

(</>) :: Key -> Key -> Key
(</>) (Key p1) (Key p2) = Key $ p1 <> p2
