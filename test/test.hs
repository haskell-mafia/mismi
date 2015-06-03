import           Disorder.Core.Main

import           Test.Mismi.Control.Amazonka
import qualified Test.Mismi.S3.Commands
import qualified Test.Mismi.S3.Control
import qualified Test.Mismi.S3.Data

main :: IO ()
main =
  disorderMain [
      Test.Mismi.Control.Amazonka.tests
    , Test.Mismi.S3.Commands.tests
    , Test.Mismi.S3.Data.tests
    , Test.Mismi.S3.Control.tests
    ]
