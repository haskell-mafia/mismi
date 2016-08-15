import           Disorder.Core.Main

import qualified Test.IO.Mismi.Autoscaling.Commands
import           Test.Mismi (enableTests)

main :: IO ()
main =
  disorderMain =<< enableTests "AWS_TEST_AUTOSCALING" [] [
      Test.IO.Mismi.Autoscaling.Commands.tests
    ]
