{-# LANGUAGE NoImplicitPrelude #-}
module Mismi.S3.Data.Component (
    -- * Types
        Component
    ,   ComponentParseError(..)
    -- * Functions
    ,   componentText
    ,   parseComponent
    ) where

import P

import Data.Char ( isControl )
import Data.Distributive ( cotraverse )
import qualified Data.Text as T

data Component = Component { _unComponent :: T.Text} deriving (Eq)

data ComponentParseError =
        ComponentParseErrorInvalidChars T.Text
    |   ComponentParseErrorEmpty

componentText :: Component -> T.Text
componentText = _unComponent

-- TESTME
parseComponent :: T.Text -> Either (ComponentParseError, T.Text) Component
parseComponent t
    | T.null t                                                  = Left (ComponentParseErrorEmpty, t)
    | T.any (not . checkComponentChar) t                        = Left (ComponentParseErrorInvalidChars $ T.filter (not . checkComponentChar) t, t)
    | otherwise                                                 = pure $ Component t

-- helpers

checkComponentChar :: Char -> Bool
checkComponentChar = cotraverse and [(/= '/'), not . isControl]
