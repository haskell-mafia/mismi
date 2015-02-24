{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Mismi.S3.Data.Component.WordTest ( tests ) where

import Mismi.S3.Data.Component.Word

import Mismi.Test
import Mismi.Test.Data.Text
import Mismi.Test.S3.Data.Component.Word
import Mismi.Arbitrary

import qualified Data.Text as T

prop_parseSafeComponent :: SafeComponentText -> Property
prop_parseSafeComponent (SafeComponentText t) = (componentWordText <$> parseComponentWord t) === pure t

prop_rejectSlashes :: SlashedComponentText -> Property
prop_rejectSlashes (SlashedComponentText t) = parseComponentWord t === Left (ComponentWordParseErrorInvalidChars (T.filter (== '/') t), t)

prop_rejectControls :: ControlCharText -> Property
prop_rejectControls (ControlCharText t) = parseComponentWord t === Left (ComponentWordParseErrorInvalidChars t, t)

prop_rejectEmptyStrings :: Property
prop_rejectEmptyStrings = parseComponentWord "" === Left (ComponentWordParseErrorEmpty, "")

return []

tests :: IO Bool
tests = $quickCheckAll
