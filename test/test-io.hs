import           Orphanarium.Core.Main

import qualified Test.IO.Mismi.S3.Commands

main :: IO ()
main =
  orphanariumMain [
    Test.IO.Mismi.S3.Commands.tests
    ]
