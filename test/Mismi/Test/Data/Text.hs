{-# LANGUAGE NoImplicitPrelude #-}
module Mismi.Test.Data.Text (
    -- * Types
        ControlCharText(..)
    ) where

import Mismi.Test

import qualified Data.Text as T

newtype ControlCharText = ControlCharText T.Text deriving (Show, Eq)

{-# ANN module ("HLint: ignore Use string literal" :: T.Text) #-}

instance Arbitrary ControlCharText where
  arbitrary = ControlCharText . T.pack <$> listOf1 (elements
    [ '\NUL'
    , '\EOT'
    , '\ACK'
    , '\SYN'
    ])
