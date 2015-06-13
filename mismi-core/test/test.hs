import           Disorder.Core.Main

import qualified Test.Mismi.Control.Amazonka

main :: IO ()
main =
  disorderMain [
      Test.Mismi.Control.Amazonka.tests
    ]
