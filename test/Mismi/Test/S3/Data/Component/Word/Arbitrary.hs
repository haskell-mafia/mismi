{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Mismi.Test.S3.Data.Component.Word.Arbitrary (
    -- * Types
        SafeComponentText(..)
    ,   SlashedComponentText(..)
    ,   ReservedComponentText(..)
    ) where

import P

import Mismi.Test

import qualified Data.Text as T

-- |
-- Text that should safely be parsed into a `ComponentWord`
--
newtype SafeComponentText = SafeComponentText T.Text deriving (Show, Eq)

instance Arbitrary SafeComponentText where
    arbitrary = SafeComponentText <$> do
        xs <- listOf1 $ elements safeWords
        return . T.concat . take 16 $ xs

-- |
-- Text that should fail to be parsed as a `ComponentWord` as it contains slashes
--
newtype SlashedComponentText = SlashedComponentText T.Text deriving (Show, Eq)

instance Arbitrary SlashedComponentText where
    arbitrary = SlashedComponentText <$> do
        xs <- listOf1 $ elements safeWords
        fs <- vectorOf (length xs) $ elements [(<> "/"), ("/" <>)]
        return . T.concat . take 16 $ zipWith id fs xs

-- |
-- Text that should fail to be parsed as a `ComponentWord` because it contains reserved characters
--
newtype ReservedComponentText = ReservedComponentText T.Text

instance Arbitrary ReservedComponentText where
    arbitrary = ReservedComponentText <$> do
        xs <- listOf1 $ elements safeWords
        fs <- vectorOf (length xs) . elements $ fmap (<>) reservedChars ++ fmap (flip (<>)) reservedChars
        return . T.concat . take 16 $ zipWith id fs xs


-- helpers

safeWords :: [T.Text]
safeWords = ["12", "34", "happy", "sad", ".", ":", "-"]

reservedChars :: [T.Text]
reservedChars = [":", "-"]
