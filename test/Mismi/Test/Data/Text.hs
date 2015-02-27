{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mismi.Test.Data.Text (
    -- * Types
        ControlCharText(..)
    ,   SlashedText(..)
    -- * Functions
    ,   wordsBy
    ) where

import Mismi.Test

import Data.List ( zipWith )
import qualified Data.List.Split as S ( wordsBy )

import qualified Data.Text as T

-- |
-- Text contains randomly interspersed slashes
--
newtype SlashedText = SlashedText T.Text deriving (Show, Eq)

-- |
-- This instance may produce text that is too large to work for integration tests...
--
-- But for pure tests, I want to test them all
--
instance Arbitrary SlashedText where
    arbitrary = SlashedText <$> do
        ps <- listOf1 arbitrary
        fs <- vectorOf (length ps) $ elements [(<> "/"), ("/" <>)]
        return . T.concat $ zipWith id fs ps


newtype ControlCharText = ControlCharText T.Text deriving (Show, Eq)

{-# ANN module ("HLint: ignore Use string literal" :: T.Text) #-}

instance Arbitrary ControlCharText where
    arbitrary = ControlCharText . T.pack <$> listOf1 (elements
        [   '\NUL'
        ,   '\EOT'
        ,   '\ACK'
        ,   '\SYN'
        ])

-- functions

wordsBy :: (Char -> Bool) -> T.Text -> [T.Text]
wordsBy f = fmap T.pack . S.wordsBy f . T.unpack
