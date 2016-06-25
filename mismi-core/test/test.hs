import           Disorder.Core.Main

import qualified Test.Mismi.Control
import qualified Test.Mismi.Data

main :: IO ()
main =
  disorderMain [
      Test.Mismi.Control.tests
    , Test.Mismi.Data.tests
    ]
