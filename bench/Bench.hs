module Main where

import           Data.Text         as T (Text)
import qualified Data.Text.IO.Utf8 as TIO (readFile)
import qualified Day21
import           System.Directory  (getHomeDirectory)
import           Test.Tasty.Bench  (Benchmark, bcompare, bench, bgroup,
                                    defaultMain, nf)

inputPath = "/adventOfCode/input/2024/day20.txt"


tests :: Text -> [Benchmark]
tests input =
  [ bench "part 1" $ nf (Day21.part1 False) input
  , bench "part 2" $ nf (Day21.part2 False) input
  ]

main :: IO ()
main = do
  home <- getHomeDirectory
  file <- TIO.readFile $ home ++ inputPath
  defaultMain $ tests file
