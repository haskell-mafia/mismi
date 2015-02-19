import           Control.Monad

import qualified Mismi.S3.CommandsTest
import qualified Mismi.S3.DataTest

import           System.Exit
import           System.IO

main :: IO ()
main =
  hSetBuffering stdout LineBuffering >> mapM id [
      Mismi.S3.CommandsTest.tests
    , Mismi.S3.DataTest.tests
    ] >>= \rs -> when (not . all id $ rs) exitFailure
