import           Disorder.Core.Main

import qualified Test.IO.Mismi.SQS.Commands
import qualified Test.IO.Mismi.SQS.Control

main :: IO ()
main =
  disorderMain [
      Test.IO.Mismi.SQS.Commands.tests
    , Test.IO.Mismi.SQS.Control.tests
    ]
