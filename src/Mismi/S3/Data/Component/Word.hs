{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude, OverloadedStrings #-}
module Mismi.S3.Data.Component.Word (
    -- * Types
        ComponentWord
    ,   ComponentWordParseError(..)
    -- * Functions
    ,   componentWordText
    ,   parseComponentWord
    ) where

import P hiding ( (<>) )

import Data.Char ( isControl )
import Data.Data ( Data )
import Data.Distributive ( cotraverse )
import Data.Semigroup ( Semigroup(..) )
import qualified Data.Text as T
import Data.Typeable ( Typeable )

data ComponentWord = ComponentWord { _unComponentWord :: T.Text } deriving (Eq, Typeable, Data)

data ComponentWordParseError =
        ComponentWordParseErrorInvalidChars T.Text
    |   ComponentWordParseErrorEmpty
        deriving (Show, Eq)

instance Show ComponentWord where
    show (ComponentWord t) = concat ["[qcomponentword|", show t,"|]"]

instance Semigroup ComponentWord where
    (ComponentWord t1) <> (ComponentWord t2) = ComponentWord $ t1 <> t2

componentWordText :: ComponentWord -> T.Text
componentWordText = _unComponentWord

-- TESTME
parseComponentWord :: T.Text -> Either (ComponentWordParseError, T.Text) ComponentWord
parseComponentWord t
    | T.null t                                  = Left (ComponentWordParseErrorEmpty, t)
    | T.any (not . checkComponentWordChar) t    = Left (ComponentWordParseErrorInvalidChars $ T.filter (not . checkComponentWordChar) t, t)
    | otherwise                                 = pure $ ComponentWord t

-- some helpful operators for some of our common applications of components

-- helpers

{-# ANN module ("HLint: ignore Use string literal" :: T.Text) #-}

-- | We use the @-@ char for our "views" for word separation
-- We use the @:@ char for our "views" for concept separation
checkComponentWordChar :: Char -> Bool
checkComponentWordChar = cotraverse and [(`notElem` ['/', '-', ':']), not . isControl]
