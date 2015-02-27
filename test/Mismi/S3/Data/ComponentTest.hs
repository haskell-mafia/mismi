{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Mismi.S3.Data.ComponentTest ( tests ) where

import Mismi.S3.Data.Component

import Mismi.Test hiding ( (<>) )
import Mismi.Test.Data.Text
import Mismi.Test.S3.Data.Component ()
import Mismi.Test.S3.Data.Component.Arbitrary

import Data.Semigroup ( (<>) )
import qualified Data.Text as T

prop_parseSafeComponent :: SafeComponentText -> Property
prop_parseSafeComponent (SafeComponentText t) = (componentText <$> parseComponent t) === pure t

prop_parseDelimitedText_ComponentsValid :: T.Text -> Property
prop_parseDelimitedText_ComponentsValid t = (=== pure (parseDelimitedText t)) . traverse (parseComponent . componentText) . parseDelimitedText $ t

-- the result of parsing a string of text into a Key should be the same regardless of how many '/'s appear consecutively
-- This prop also shows that the reasult is also invariant to any '/'s that appear at the front or the end....
--
prop_parseDelimitedText_RepeatedSlashes :: SlashedText -> Property
prop_parseDelimitedText_RepeatedSlashes (SlashedText t) =
    let
        squashSlashes :: T.Text -> T.Text
        squashSlashes = T.intercalate "/" . wordsBy (== '/')

        compareParseResults :: T.Text -> T.Text -> Property
        compareParseResults = (===) `on` parseDelimitedText

        compareSquashedResults :: T.Text -> Property
        compareSquashedResults = squashSlashes >>= compareParseResults

    in compareSquashedResults t

prop_rejectSlashes :: SlashedComponentText -> Property
prop_rejectSlashes (SlashedComponentText t) = parseComponent t === Left (ComponentParseErrorInvalidChars (T.filter (== '/') t), t)

-- We probably wouldnt put these in our own S3 paths, but we want to be able to
-- "support" weird corner cases like this that might occur in buckets thats are
-- maintained by others.
--
prop_parseAcceptControls :: ControlCharText -> Property
prop_parseAcceptControls (ControlCharText t) = (componentText <$> parseComponent t) === pure t

prop_rejectEmptyStrings :: Property
prop_rejectEmptyStrings = parseComponent "" === Left (ComponentParseErrorEmpty, "")

prop_ComponentAndTextConcatDistributive :: Component -> Component -> Property
prop_ComponentAndTextConcatDistributive c1 c2 = componentText (c1 <> c2) ===  componentText c1 `T.append` componentText c2

prop_WeakInverse :: Component -> Property
prop_WeakInverse word = parseComponent (componentText word) === pure word

-- The arbitrary instance for `Component` uses @<>@ to generate examples but I dont believe this affects the integrity of the test
-- but rather in combination with `prop_WeakInverse` validates the arbitrary instance (this isnt circular since `parseComponent` is independent,
-- and the implementations are not correlated)
--
prop_concatClosed :: Component -> Component -> Property
prop_concatClosed c1 c2 = parseComponent (componentText (c1 <> c2)) === pure (c1 <> c2)

return []

tests :: IO Bool
tests = $quickCheckAll
