import           Disorder.Core.Main

import qualified Test.Mismi.Autoscaling.Core.Data

main :: IO ()
main =
  disorderMain [
      Test.Mismi.Autoscaling.Core.Data.tests
    ]
