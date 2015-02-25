{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Mismi.S3.Data.Component.WordTest ( tests ) where

import Mismi.S3.Data.Component.Word

import Mismi.Test hiding ( (<>) )
import Mismi.Test.Data.Text
import Mismi.Test.S3.Data.Component.Word ()
import Mismi.Test.S3.Data.Component.Word.Arbitrary

import Data.Semigroup ( (<>) )
import qualified Data.Text as T

prop_parseSafeComponent :: SafeComponentText -> Property
prop_parseSafeComponent (SafeComponentText t) = (componentWordText <$> parseComponentWord t) === pure t

prop_rejectSlashes :: SlashedComponentText -> Property
prop_rejectSlashes (SlashedComponentText t) = parseComponentWord t === Left (ComponentWordParseErrorInvalidChars (T.filter (== '/') t), t)

prop_rejectControls :: ControlCharText -> Property
prop_rejectControls (ControlCharText t) = parseComponentWord t === Left (ComponentWordParseErrorInvalidChars t, t)

prop_rejectEmptyStrings :: Property
prop_rejectEmptyStrings = parseComponentWord "" === Left (ComponentWordParseErrorEmpty, "")

prop_ComponentWordAndTextConcatDistributive :: ComponentWord -> ComponentWord -> Property
prop_ComponentWordAndTextConcatDistributive c1 c2 = componentWordText (c1 <> c2) ===  componentWordText c1 `T.append` (componentWordText c2)

prop_WeakInverse :: ComponentWord -> Property
prop_WeakInverse word = parseComponentWord (componentWordText word) === pure word

-- The arbitrary instance for `ComponentWord` uses @<>@ to generate examples but I dont believe this affects the integrity of the test
-- but rather in combination with `prop_WeakInverse` validates the arbitrary instance (this isnt circular since `parseComponentWord` is independent,
-- and the implementations are not correlated)
--
prop_concatClosed :: ComponentWord -> ComponentWord -> Property
prop_concatClosed c1 c2 = parseComponentWord (componentWordText (c1 <> c2)) === pure (c1 <> c2)

return []

tests :: IO Bool
tests = $quickCheckAll
