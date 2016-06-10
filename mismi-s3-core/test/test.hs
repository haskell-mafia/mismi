import           Disorder.Core.Main

import qualified Test.Mismi.S3.Core.Data


main :: IO ()
main =
  disorderMain [
      Test.Mismi.S3.Core.Data.tests
    ]
