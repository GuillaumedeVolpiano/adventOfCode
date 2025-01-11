module Main where

import           Control.Monad.State (evalState)
import           Data.Either         (fromRight)
import           Data.Text           as T (Text, pack)
import qualified Data.Text.IO.Utf8   as TIO (readFile)
import qualified Day6
import           System.Directory    (getHomeDirectory)
import           Test.Tasty.Bench    (Benchmark, bcompare, bench, bgroup,
                                      defaultMain, nf)
import           Text.Megaparsec     (runParserT)

inputPath = "/adventOfCode/input/2015/day6.txt"

op x = flip evalState x . runParserT Day6.parseInput "Benching"

tests :: Text -> [Benchmark]
tests input =
  [ bench "simple map" $ nf (op Day6.dlempty) input
  , bench "intmap" $ nf (op Day6.bdlempty) input
  , bench "intmultiset" $ nf (op Day6.obdlempty) input
  ]

main :: IO ()
main = do
  home <- getHomeDirectory
  file <- TIO.readFile $ home ++ inputPath
  defaultMain $ tests file
