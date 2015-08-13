import           Disorder.Core.Main

import qualified Test.IO.Mismi.S3.Aws.Commands
import qualified Test.IO.Mismi.S3.Commands
import qualified Test.IO.Mismi.S3.Control
import qualified Test.IO.Mismi.S3.Internal


main :: IO ()
main =
  disorderMain [
      Test.IO.Mismi.S3.Aws.Commands.tests
    , Test.IO.Mismi.S3.Commands.tests
    , Test.IO.Mismi.S3.Control.tests
    , Test.IO.Mismi.S3.Internal.tests
    ]
