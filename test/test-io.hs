import           Disorder.Core.Main

import qualified Test.IO.Mismi.S3.Commands

main :: IO ()
main =
  disorderMain [
    Test.IO.Mismi.S3.Commands.tests
    ]
