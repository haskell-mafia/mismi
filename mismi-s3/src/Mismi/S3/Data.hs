{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
module Mismi.S3.Data (
    WriteMode(..)
  , SyncMode (..)
  , Bucket(..)
  , Address (..)
  , Key (..)
  , (</>)
  , dirname
  , foldWriteMode
  , foldSyncMode
  , basename
  , addressFromText
  , addressToText
  , removeCommonPrefix
  , withKey
  , s3Parser
  ) where

import           Data.Align
import           Data.Attoparsec.Text hiding (parse, Fail )
import qualified Data.Attoparsec.Text as AT
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.List (init)
import           Data.String

import           P

-- |
-- Describes the semantics for destructive operation that may result in overwritten files.
--
data WriteMode =
      Fail        -- ^ Fail rather than overwrite any data.
    | Overwrite   -- ^ Overwrite existing data silently, i.e. we really want to do this.
    deriving (Eq, Show)

foldWriteMode :: a -> a -> WriteMode -> a
foldWriteMode f o = \case
  Fail -> f
  Overwrite -> o

data SyncMode =
  FailSync
  | OverwriteSync
  | SkipSync
  deriving (Eq, Show)

foldSyncMode :: a -> a -> a -> SyncMode -> a
foldSyncMode f o s = \case
  FailSync -> f
  OverwriteSync -> o
  SkipSync -> s

newtype Bucket = Bucket {
    unBucket :: Text
  } deriving (Eq, Show, Ord)

data Address = Address {
    bucket :: Bucket
  , key :: Key
  } deriving (Eq, Ord)

-- NOTE: This is not a "safe" data type, and makes no guarantee about what is _actually_ supported for S3
-- https://github.com/ambiata/mismi/issues/2
newtype Key = Key {
    unKey :: Text
  } deriving (Eq, Show, Ord)

instance Show Address where
  show (Address b k) =
    "Address (" <> show b <> ") (" <> show k <> ")"


(</>) :: Key -> Key -> Key
(</>) (Key p1) (Key p2) =
  if  "/" `T.isSuffixOf` p1 || p1 == "" || "/" `T.isPrefixOf` p2
    then Key $ p1 <> p2
    else Key $ p1 <> "/" <> p2

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

-- prefix key
removeCommonPrefix :: Address -> Address -> Maybe Key
removeCommonPrefix prefix addr =
  let dropMaybe :: String -> String -> Maybe Text
      dropMaybe x y =
        bool
          Nothing
          (Just . T.pack $ drop (length y) x)
          (check x y)
      check :: String -> String -> Bool
      check x y =
        all (\(l, r) -> Just l == r) (rpadZip y x)
  in
  if bucket addr == bucket prefix
     then
       if unKey (key prefix) == ""
          then
            Just $ key addr
          else
            let bk = unKey (key prefix)
                b = bool (bk <> "/") bk ("/" `T.isSuffixOf` bk)
                pk = T.unpack b
                kk = T.unpack (unKey $ key addr)
            in
              Key <$> dropMaybe kk pk
     else
       Nothing

addressToText :: Address -> Text
addressToText a =
  "s3://" <> unBucket (bucket a) <> "/" <> unKey (key a)

addressFromText :: Text -> Maybe Address
addressFromText =
  rightToMaybe . AT.parseOnly s3Parser

s3Parser :: Parser Address
s3Parser =
  s3Parser' <|> s3Parser''

s3Parser' :: Parser Address
s3Parser' = do
  _ <- string "s3://"
  b <- manyTill anyChar (char '/')
  k <- many anyChar
  pure $ Address (Bucket . T.pack $ b) (Key . T.pack $ k)

s3Parser'' :: Parser Address
s3Parser'' = do
  _ <- string "s3://"
  b <- takeWhile (/= '/')
  endOfInput
  pure $ Address (Bucket b) (Key "")
