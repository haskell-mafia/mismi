import           Disorder.Core.Main

import qualified Test.Reliability.Mismi.S3.Commands
import qualified Test.Reliability.Mismi.S3.Amazonka


main :: IO ()
main =
  disorderMain [
      Test.Reliability.Mismi.S3.Commands.tests
    , Test.Reliability.Mismi.S3.Amazonka.tests
    ]
