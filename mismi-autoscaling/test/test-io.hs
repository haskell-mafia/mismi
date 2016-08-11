import           Disorder.Core.Main

import qualified Test.IO.Mismi.Autoscaling.Commands

main :: IO ()
main =
  disorderMain [
      Test.IO.Mismi.Autoscaling.Commands.tests
    ]
