{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Mismi.S3.Data (
    WriteMode(..)
  , Bucket(..)
  , Address (..)
  , Key (..)
  , (</>)
  , dirname
  , basename
  , addressFromText
  , addressToText
  , withKey
  , s3Parser
  ) where

import           Data.Attoparsec.Text hiding (parse)
import qualified Data.Attoparsec.Text as AT
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.List (init)

import           P
import           Prelude (reverse)

-- |
-- Describes the behaviour to display when a write is attempted on a location where an object already exists.
--
data WriteMode =
        Fail        -- ^ Returns an error if there is already an object at the given address.
    |   Overwrite   -- ^ If an object already exists at the given address, overwrite it
        deriving (Eq, Show)

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

withKey :: (Key -> Key) -> Address -> Address
withKey f (Address b k) = Address b $ f k

dirname :: Key -> Key
dirname =
  Key . T.intercalate "/" . init . T.split (=='/') . unKey

-- | Get the basename for a given key (eg. basename "/foo/bar" == "bar").
--   Return 'Nothing' for the empty 'Key' _and_ when the name ends with a '/'.
basename :: Key -> Maybe Text
basename =
  mfilter (not . T.null) . listToMaybe . reverse . T.split (== '/') . unKey

addressToText :: Address -> Text
addressToText a =
  "s3://" <> unBucket (bucket a) <> "/" <> unKey (key a)

addressFromText :: Text -> Maybe Address
addressFromText =
  rightToMaybe . AT.parseOnly s3Parser

s3Parser :: Parser Address
s3Parser = do
  _ <- string "s3://"
  b <- manyTill anyChar (char '/')
  k <- many anyChar
  pure $ Address (Bucket . T.pack $ b) (Key . T.pack $ k)
