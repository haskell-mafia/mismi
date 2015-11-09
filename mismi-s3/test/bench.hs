{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Catch

import           Criterion.Main

import qualified Data.Text as T

import           Mismi.S3

import           P

import           System.IO
import           System.IO.Error
import           System.IO.Temp

import           Test.Mismi

import           Test.QuickCheck.Instances ()

import           X.Control.Monad.Trans.Either (EitherT, eitherT, mapEitherT)

createFiles :: Address -> Int -> AWS ()
createFiles prefix n = do
  mapM_ (flip (writeWithModeOrFail Overwrite) "data") $ files prefix n

createLargeFiles :: Address -> Int -> AWS ()
createLargeFiles prefix n = do
  withSystemTempFile "file" $ \f h -> do
    liftIO $ hSetFileSize h (100 * 1024 * 1024 :: Integer) {-- 100 mb --}
    liftIO $ hClose h
    mapM_ (uploadWithModeOrFail Overwrite f) $ files prefix n

files :: Address -> Int -> [Address]
files prefix n =
  fmap (\i -> withKey (</> Key ("file-" <> (T.pack $ show i))) prefix) [1..n]

run :: AWS a -> IO a
run =
  runAWSDefaultRegion

runE :: EitherT SyncError AWS a -> IO a
runE =
  eitherT (throwM . userError . T.unpack . renderSyncError) pure . mapEitherT runAWSDefaultRegion

main :: IO ()
main = do
  let a = Address (Bucket "ambiata-dev-view") (Key "s3-benchmarks/small-foo")
      b = Address (Bucket "ambiata-dev-view") (Key "s3-benchmarks/small-bar")
      c = Address (Bucket "ambiata-dev-view") (Key "s3-benchmarks/large-foo")
      d = Address (Bucket "ambiata-dev-view") (Key "s3-benchmarks/large-bar")
      o = Address (Bucket "ambiata-dev-view") (Key "s3-benchmarks/output")
  run $ do
    createFiles a 100
    createFiles b 1000
    createLargeFiles c 100
    createLargeFiles d 1000
  runE $ syncWithMode OverwriteSync b o 100
  defaultMain [
      bgroup "sync-small-files" [
          bench "sem-100-100" (nfIO . runE $ syncWithMode OverwriteSync a o 100)
        , bench "sem-100-50" (nfIO . runE $ syncWithMode OverwriteSync a o 50)
        , bench "sem-100-20" (nfIO . runE $ syncWithMode OverwriteSync a o 20)
        , bench "sem-100-10" (nfIO . runE $ syncWithMode OverwriteSync a o 10)
        , bench "sem-100-1" (nfIO . runE $ syncWithMode OverwriteSync a o 1)
        , bench "sem-1000-1000" (nfIO . runE $ syncWithMode OverwriteSync b o 1000)
        , bench "sem-1000-100" (nfIO . runE $ syncWithMode OverwriteSync b o 100)
        , bench "sem-1000-10" (nfIO . runE $ syncWithMode OverwriteSync b o 10)
        ]
    , bgroup "sync-large-files" [
          bench "sem-100-100" (nfIO . runE $ syncWithMode OverwriteSync c o 100)
        , bench "sem-100-50" (nfIO . runE $ syncWithMode OverwriteSync c o 50)
        , bench "sem-100-20" (nfIO . runE $ syncWithMode OverwriteSync c o 20)
        , bench "sem-100-10" (nfIO . runE $ syncWithMode OverwriteSync c o 10)
        , bench "sem-100-1" (nfIO . runE $ syncWithMode OverwriteSync c o 1)
        , bench "sem-1000-1000" (nfIO . runE $ syncWithMode OverwriteSync d o 1000)
        , bench "sem-1000-100" (nfIO . runE $ syncWithMode OverwriteSync d o 100)
        , bench "sem-1000-10" (nfIO . runE $ syncWithMode OverwriteSync d o 10)
        ]

    ]
