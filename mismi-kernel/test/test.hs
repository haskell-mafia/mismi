import           Disorder.Core.Main
import qualified Test.Mismi.Kernel.Data
main :: IO ()
main =
  disorderMain [
      Test.Mismi.Kernel.Data.tests
    ]
