import           Disorder.Core.Main

import qualified Test.Reliability.Mismi.S3.Aws.Commands
import qualified Test.Reliability.Mismi.S3.Commands


main :: IO ()
main =
  disorderMain [
      Test.Reliability.Mismi.S3.Aws.Commands.tests
    , Test.Reliability.Mismi.S3.Commands.tests
    ]
