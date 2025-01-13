module Main where

import           Data.ByteString  (ByteString)
import qualified Data.ByteString  as B (putStr, readFile)
import           Day12
import           System.Directory (getHomeDirectory)
import           Test.Tasty.Bench (Benchmark, bcompare, bench, bgroup,
                                   defaultMain, nf, nfIO)

inputPath = "/adventOfCode/input/2015/day12.txt"

tests :: ByteString -> [Benchmark]
tests input =
  [ bench "Part 1" $ nf (part1 True) input
  , bench "Part 2" $ nf (part2 True) input
  ]

main :: IO ()
main = do
  home <- getHomeDirectory
  file <- B.readFile $ home ++ inputPath
  defaultMain . tests $ file
