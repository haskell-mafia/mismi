import           Control.Monad

import           System.Exit
import           System.IO

main :: IO ()
main =
  hSetBuffering stdout LineBuffering >> mapM id [
    ] >>= \rs -> when (not . all id $ rs) exitFailure
