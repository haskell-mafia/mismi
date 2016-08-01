import           Disorder.Core.Main

import qualified Test.Mismi.EC2.Core.Data

main :: IO ()
main =
  disorderMain [
      Test.Mismi.EC2.Core.Data.tests
    ]
