{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Mismi.S3.Data.Component.ConceptTest ( tests ) where

import P

import Data.Bifunctor ( first )

import Mismi.S3.Data.Component.Concept
import Mismi.S3.Data.Component.Word

import Mismi.Test
import Mismi.Test.Data.Text
import Mismi.Test.S3.Data.Component.Concept ()
import Mismi.Test.S3.Data.Component.Concept.Arbitrary
import Mismi.Test.S3.Data.Component.Word ()

import qualified Data.Text as T

prop_parseSafeComponent :: SafeConceptText -> Property
prop_parseSafeComponent (SafeConceptText t) = (componentConceptText <$> parseComponentConcept t) === pure t

prop_rejectSlashes :: SlashedConceptText -> Property
prop_rejectSlashes (SlashedConceptText t) = first (errorType . fst) (parseComponentConcept t) === Left "Invalid Chars"

prop_rejectControls :: ControlCharText -> Property
prop_rejectControls (ControlCharText t) = parseComponentWord t === Left (ComponentWordParseErrorInvalidChars t, t)

prop_rejectEmptyStrings :: Property
prop_rejectEmptyStrings = parseComponentConcept "" === Left (ComponentWordParseErrorEmpty, "")

prop_ComponentConceptRepresentation :: ComponentConcept -> ComponentWord -> Property
prop_ComponentConceptRepresentation c1 c2 = componentConceptText (c1 <-> c2) ===  T.concat [componentConceptText c1, "-", componentWordText c2]

prop_WeakInverse :: ComponentConcept -> Property
prop_WeakInverse concept = parseComponentConcept (componentConceptText concept) === pure concept

-- The arbitrary instance for `ComponentWord` uses @<>@ to generate examples but I dont believe this affects the integrity of the test
-- but rather in combination with `prop_WeakInverse` validates the arbitrary instance (this isnt circular since `parseComponentWord` is independent,
-- and the implementations are not correlated)
--
prop_HyphenClosed :: ComponentConcept -> ComponentWord -> Property
prop_HyphenClosed c1 c2 = parseComponentConcept (componentConceptText (c1 <-> c2)) === pure (c1 <-> c2)

-- helpers

errorType :: ComponentWordParseError -> T.Text
errorType (ComponentWordParseErrorInvalidChars {})  = "Invalid Chars"
errorType ComponentWordParseErrorEmpty              = "Empty Word"

return []

tests :: IO Bool
tests = $quickCheckAll
