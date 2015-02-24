{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
module Mismi.S3.Data.Component.QQ (
    -- * QuasiQuoters
        qcomponentword
    ) where

import P

import Mismi.S3.Data.Component.Word

import qualified Prelude as P ( error )
import Data.String ( String )
import qualified Data.Text as T

import Language.Haskell.TH ( ExpQ, Lit(..), appE, conE, varE, litE )
import Language.Haskell.TH.Quote ( QuasiQuoter(..), dataToExpQ )

qcomponentword :: QuasiQuoter
qcomponentword = QuasiQuoter
    {   quoteExp    = componentExp
    ,   quotePat    = P.error "not able to qq pats"
    ,   quoteType   = P.error "not able to qq types"
    ,   quoteDec    = P.error "not able to qq decs"
    }
    where
        componentExp :: String -> ExpQ
        componentExp = either (P.error . show) (const Nothing) . parseComponentWord . T.pack

