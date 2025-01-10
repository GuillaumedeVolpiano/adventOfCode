module Main where

import           Data.Text         as T (Text, pack)
import qualified Data.Text.IO.Utf8 as TIO (readFile)
import qualified Day5
import           MD5
import           System.Directory  (getHomeDirectory)
import           Test.Tasty.Bench  (Benchmark, bcompare, bench, bgroup,
                                    defaultMain, nf)
import           TextShow          (showt)

inputPath = "/adventOfCode/input/2016/day5.txt"

testRun = [527000 .. 5278568] :: [Int]

testEntry = pack "abc"

tests :: Text -> [Benchmark]
tests input =
  [bench "md5concat" $ nf (map (flip md5Concat testEntry . showt)) testRun]

main :: IO ()
main = do
  home <- getHomeDirectory
  file <- TIO.readFile $ home ++ inputPath
  defaultMain $ tests file
