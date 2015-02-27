{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Mismi.Test.S3.Data.Component (
    -- * Types
    ) where

import P

import Mismi.Test

import Mismi.S3.Data.Component
import Mismi.S3.Data.Component.QQ

import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Semigroup ( sconcat )

instance Arbitrary Component where
    arbitrary =
        let
            w = elements testComponents
        in do
            ws <- (:|) <$> w <*> listOf w
            return $ sconcat ws

testComponents :: [Component]
testComponents =
    [   [qcomponent|happy|]
    ,   [qcomponent|sad|]
    ,   [qcomponent|foo|]
    ,   [qcomponent|bar|]
    ,   [qcomponent|stark|]
    ,   [qcomponent|quill|]
    ,   [qcomponent|hankpym|]
    ,   [qcomponent|lukecage|]
    ,   [qcomponent|miguel|]
    ,   [qcomponent|.|]
    ,   [qcomponent|-|]
    ,   [qcomponent|:|]
    ]
