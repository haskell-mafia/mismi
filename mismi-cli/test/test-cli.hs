import           Disorder.Core.Main

main :: IO ()
main =
  disorderCliMain ["./dist/build/s3/s3"]
