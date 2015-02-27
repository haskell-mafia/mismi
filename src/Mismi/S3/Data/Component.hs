{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude, OverloadedStrings #-}
module Mismi.S3.Data.Component (
    -- * Types
        Component
    ,   ComponentParseError(..)
    -- * Functions
    ,   componentText
    ,   parseComponent
    ,   parseDelimitedText
    ) where

import P hiding ( (<>) )

import Data.Char ( isControl )
import Data.Data ( Data )
import Data.Semigroup ( Semigroup(..) )
import qualified Data.Text as T
import Data.Typeable ( Typeable )

data Component = Component { _unComponent :: T.Text } deriving (Eq, Typeable, Data)

data ComponentParseError =
        ComponentParseErrorInvalidChars T.Text
    |   ComponentParseErrorEmpty
        deriving (Show, Eq)

instance Show Component where
    show (Component t) = concat ["[qcomponent|", T.unpack t,"|]"]

instance Semigroup Component where
    (Component t1) <> (Component t2) = Component $ t1 <> t2

componentText :: Component -> T.Text
componentText = _unComponent

parseComponent :: T.Text -> Either (ComponentParseError, T.Text) Component
parseComponent t
    | T.null t                                  = Left (ComponentParseErrorEmpty, t)
    | T.any (not . checkComponentChar) t        = Left (ComponentParseErrorInvalidChars $ T.filter (not . checkComponentChar) t, t)
    | otherwise                                 = pure $ Component t

parseDelimitedText :: T.Text -> [Component]
parseDelimitedText = fmap Component . filter (not . T.null) . T.split (== '/')

-- some helpful operators for some of our common applications of components

-- helpers

checkComponentChar :: Char -> Bool
checkComponentChar = (/= '/')
