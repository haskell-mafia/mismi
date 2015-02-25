{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mismi.S3.Data.Component (
    -- * Types
        ComponentConcept(..)
    -- * Operators
    ,   (<:>)
    -- * Functions
    ,   componentText
    ,   parseComponent
    ) where

import P

import Mismi.S3.Data.Component.Concept
import Mismi.S3.Data.Component.Word

import Data.List.NonEmpty ( head, nonEmpty, tail )
import qualified Data.Text as T

infixl 6 :::
infixl 6 <:>

-- |
-- Essentially a Reverse NonEmpty Snoc List
--
data Component =
        SingleConcept ComponentConcept
    |   Component ::: ComponentConcept

(<:>) :: Component -> ComponentConcept -> Component
(<:>) = (:::)

-- TESTME
componentText :: Component -> T.Text
componentText (SingleConcept concept) = componentConceptText concept
componentText (component ::: concept) = T.concat [componentText component, ":", componentConceptText concept]

-- TESTME
parseComponent :: T.Text -> Either (ComponentWordParseError, T.Text) Component
parseComponent t = do
    cs <- traverse parseComponentConcept . T.split (== ':') $ t
    nonEmptyComps <- maybe (Left (ComponentWordParseErrorEmpty, t)) return . nonEmpty $ cs
    return $ foldl' (<:>) (SingleConcept $ head nonEmptyComps) $ tail nonEmptyComps
