{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mismi.S3.Data.Component.Concept (
    -- * Types
        ComponentConcept(..)
    -- * Operators
    ,   (<->)
    -- * Functions
    ,   componentConceptText
    ,   parseComponentConcept
    ) where

import P

import Mismi.S3.Data.Component.Word

import Data.List.NonEmpty ( head, nonEmpty, tail )
import qualified Data.Text as T

infixl 5 :-:
infixl 5 <->

-- |
-- Essentially a Reverse NonEmpty Snoc List
--
data ComponentConcept =
        SingleWord ComponentWord
    |   ComponentConcept :-: ComponentWord
        deriving (Show, Eq)

(<->) :: ComponentConcept -> ComponentWord -> ComponentConcept
(<->) = (:-:)

-- TESTME
componentConceptText :: ComponentConcept -> T.Text
componentConceptText (SingleWord word)  = componentWordText word
componentConceptText (c :-: w)          = T.concat [componentConceptText c, "-", componentWordText w]

-- TESTME
parseComponentConcept :: T.Text -> Either (ComponentWordParseError, T.Text) ComponentConcept
parseComponentConcept t = do
    ws <- traverse parseComponentWord . T.split (== '-') $ t
    nonEmptyWords <- maybe (Left (ComponentWordParseErrorEmpty, t)) return . nonEmpty $ ws
    return $ foldl' (<->) (SingleWord $ head nonEmptyWords) $ tail nonEmptyWords
