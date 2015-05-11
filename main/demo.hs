{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           System.Environment
import           BuildInfo_mismi
import           Mismi.S3.Control
import           Mismi.S3.Commands
import           Mismi.S3.Data
import qualified Data.Text as T
import           Data.Monoid

main :: IO ()
main = getArgs >>= \args -> case args of
  "upload" : source : destination : [] ->
    run destination $ upload source
  "download" : source : destination : [] ->
    run source $ flip download destination
  "--version" : [] ->
    putStrLn $ "version: " ++ buildInfoVersion
  _ ->
    usage

run :: String -> (Address -> S3Action a) -> IO ()
run d f = case addressFromText $ T.pack d of
  Just d' -> void . runS3WithDefaults $ f d'
  Nothing -> putStrLn $ "Failed to parse address: " <> d

usage :: IO ()
usage = do
  putStrLn "usage: mismi-demo upload [souce file] [s3 destination uri]"
  putStrLn "  e.g: mismi-demo upload /tmp/test.txt s3://ambiata-dev-view/test-files/test.txt"
  putStrLn ""
  putStrLn "usage: mismi-demo download [s3 source uri] [local file]"
  putStrLn "  e.g: mismi-demo download s3://ambiata-dev-view/test-files/test.txt /tmp/test.txt"
  putStrLn ""
