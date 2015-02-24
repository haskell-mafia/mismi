{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Mismi.Test.S3.Data.Component.Word (
    -- * Types
    ) where

import P

import Mismi.Test

import Mismi.S3.Data.Component.Word
import Mismi.S3.Data.Component.QQ

import Data.List.NonEmpty ( fromList )
import Data.Semigroup ( sconcat )

instance Arbitrary ComponentWord where
    arbitary = do
        ws <- fromList . listOf1 $ elements testComponents -- listOf1 returning [] is stupid.. (`fromList` is unsafe...)
        return $ sconcat ws

testComponents :: [ComponentWord]
testComponents =
    [   [qcomponentword|"happy"|]
    ,   [qcomponentword|"sad"|]
    ,   [qcomponentword|"foo"|]
    ,   [qcomponentword|"bar"|]
    ,   [qcomponentword|"stark"|]
    ,   [qcomponentword|"quill"|]
    ,   [qcomponentword|"hankpym"|]
    ,   [qcomponentword|"lukecage"|]
    ,   [qcomponentword|"miguel"|]
    ,   [qcomponentword|"."|]
    ]
