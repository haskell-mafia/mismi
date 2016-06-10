import           Disorder.Core.Main

import qualified Test.Mismi.S3.Commands
import qualified Test.Mismi.S3.Control
import qualified Test.Mismi.S3.Internal


main :: IO ()
main =
  disorderMain [
      Test.Mismi.S3.Commands.tests
    , Test.Mismi.S3.Control.tests
    , Test.Mismi.S3.Internal.tests
    ]
