import           Disorder.Core.Main

import qualified Test.Mismi.S3.Commands
import qualified Test.Mismi.S3.Data
import qualified Test.Mismi.SQS.Commands


main :: IO ()
main =
  disorderMain [
      Test.Mismi.S3.Commands.tests
    , Test.Mismi.S3.Data.tests
    , Test.Mismi.SQS.Commands.tests
    ]
