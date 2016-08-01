import           Disorder.Core.Main

import qualified Test.Mismi.EC2.Data

main :: IO ()
main =
  disorderMain [
      Test.Mismi.EC2.Data.tests
    ]
