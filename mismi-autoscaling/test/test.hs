import           Disorder.Core.Main

import qualified Test.Mismi.Autoscaling.Data

main :: IO ()
main =
  disorderMain [
      Test.Mismi.Autoscaling.Data.tests
    ]
