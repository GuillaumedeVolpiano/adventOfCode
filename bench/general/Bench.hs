{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Day3
import qualified Streamly.Data.Stream       as S (fromList)
import           System.Directory           (getHomeDirectory)
import           Test.Tasty.Bench           (Benchmark, bench, defaultMain,
                                             whnfIO, env)
import qualified Data.ByteString as BS (readFile, unpack)
import Data.ByteString (ByteString)
import Data.Function ((&))

inputPath :: String
inputPath = "/github/adventOfCode/input/2025/day3.txt"

tests :: IO ByteString -> [Benchmark]
tests input =
  [ env input $ \bs -> bench "Part 1" $ whnfIO (S.fromList (BS.unpack bs) & Day3.lowJoltage)
  , env input $ \bs -> bench "Part 2" $ whnfIO (S.fromList (BS.unpack bs) & Day3.highJoltage)]

main :: IO ()
main = do
  home <- getHomeDirectory
  let file = BS.readFile $ home ++ inputPath
  defaultMain . tests $ file
