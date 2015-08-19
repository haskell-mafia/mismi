import           Disorder.Core.Main

import qualified Test.Mismi.Control

main :: IO ()
main =
  disorderMain [
      Test.Mismi.Control.tests
    ]
