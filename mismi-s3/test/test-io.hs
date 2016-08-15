import           Disorder.Core.Main

import qualified Test.IO.Mismi.S3.Commands
import qualified Test.IO.Mismi.S3.Internal
import           Test.Mismi (enableTests)

main :: IO ()
main =
  disorderMain =<< enableTests "AWS_TEST_S3" [
      Test.IO.Mismi.S3.Internal.tests
    ] [
      Test.IO.Mismi.S3.Internal.tests
    , Test.IO.Mismi.S3.Commands.tests
    ]
