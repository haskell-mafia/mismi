import           Disorder.Core.Main

import qualified Test.IO.Mismi.IAM.Commands


main :: IO ()
main =
  disorderMain [
      Test.IO.Mismi.IAM.Commands.tests
    ]
