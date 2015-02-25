{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
module Mismi.S3.Data.Component.QQ (
    -- * QuasiQuoters
        qcomponentword
    ) where

import P

import Mismi.S3.Data.Component.Word

import qualified Prelude as P ( error )
import Data.Generics ( extQ )
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
        -- `Data.Data.Data` instance for `Data.Text.Text` seems hideously broken, I get the following error when using it:
        --
        -- @
        -- Illegal data constructor name: ‘pack’
        --     When splicing a TH expression:
        --           Mismi.S3.Data.Component.Word.ComponentWord (Data.Text.Internal.pack ((GHC.Types.:) '"' ((GHC.Types.:) 'h' ((GHC.Types.:) 'a' ((GHC.Types.:) 'p' ((GHC.Types.:) 'p' ((GHC.Types.:) 'y' ((GHC.Types.:) '"' GHC.Types.[]))))))))
        -- @
        --
        -- hence `textExp`
        --
        textExp :: T.Text -> Maybe ExpQ
        textExp = pure . appE (varE 'T.pack) . litE . StringL . T.unpack

        componentExp :: String -> ExpQ
        componentExp s = either (P.error . show) (dataToExpQ (const Nothing `extQ` textExp)) (parseComponentWord (T.pack s) :: Either (ComponentWordParseError, T.Text) ComponentWord)

