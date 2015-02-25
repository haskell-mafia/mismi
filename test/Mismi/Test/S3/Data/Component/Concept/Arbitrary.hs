{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module Mismi.Test.S3.Data.Component.Concept.Arbitrary (
    -- * Types
        SafeConceptText(..)
    ,   SlashedConceptText(..)
    ,   ReservedConceptText(..)
    ) where

import P

import Mismi.Test

import Data.List ( (++), take, zipWith )
import qualified Data.Text as T

-- |
-- Text that should safely be parsed into a `ComponentWord`
--
newtype SafeConceptText = SafeConceptText T.Text deriving (Show, Eq)

instance Arbitrary SafeConceptText where
    arbitrary = SafeConceptText <$> do
        xs <- listOf1 $ elements safeConceptPieces
        return . T.concat . take 16 $ xs

-- |
-- Text that should fail to be parsed as a `ComponentConcept` as it contains slashes
--
newtype SlashedConceptText = SlashedConceptText T.Text deriving (Show, Eq)

instance Arbitrary SlashedConceptText where
    arbitrary = SlashedConceptText <$> do
        xs <- listOf1 $ elements safeConceptPieces
        fs <- vectorOf (length xs) $ elements [(<> "/"), ("/" <>)]
        return . T.concat . take 16 $ zipWith id fs xs

-- |
-- Text that should fail to be parsed as a `ComponentWord` because it contains reserved characters
--
newtype ReservedConceptText = ReservedConceptText T.Text

instance Arbitrary ReservedConceptText where
    arbitrary = ReservedConceptText <$> do
        xs <- listOf1 $ elements safeConceptPieces
        fs <- vectorOf (length xs) . elements $ fmap (<>) reservedChars ++ fmap (flip (<>)) reservedChars
        return . T.concat . take 16 $ zipWith id fs xs


-- helpers

safeConceptPieces :: [T.Text]
safeConceptPieces = ["12", "34", "happy", "sad", "s-a"]

reservedChars :: [T.Text]
reservedChars = [":"]
