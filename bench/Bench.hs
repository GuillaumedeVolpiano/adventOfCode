module Main where

import           Data.Text         as T (Text)
import qualified Data.Text.IO.Utf8 as TIO (readFile)
import qualified Day22
import           System.Directory  (getHomeDirectory)
import           Test.Tasty.Bench  (Benchmark, bcompare, bench, bgroup,
                                    defaultMain, nf)

inputPath = "/adventOfCode/input/2024/day22.txt"


tests :: Text -> [Benchmark]
tests input =
  [ bench "part 1" $ nf (Day22.part1 False) input
  , bench "part 2" $ nf (Day22.part2 False) input
  ]

main :: IO ()
main = do
  home <- getHomeDirectory
  file <- TIO.readFile $ home ++ inputPath
  defaultMain $ tests file
