module Main where

import           Data.Word                  (Word8)
import qualified Day1
import qualified Streamly.Data.Stream       as S (fold)
import           Streamly.Data.Stream       (Stream)
import           Streamly.FileSystem.FileIO as S (read)
import           Streamly.FileSystem.Path   as S (fromString_)
import           System.Directory           (getHomeDirectory)
import           Test.Tasty.Bench           (Benchmark, bench, defaultMain,
                                             whnfIO)

inputPath :: String
inputPath = "/github/adventOfCode/input/2025/day1.txt"

tests :: Stream IO Word8 -> [Benchmark]
tests input =
  [ bench "Part 1" $ whnfIO (S.fold (Day1.findPassword Day1.calc) input)
  , bench "Part 2" $ whnfIO (S.fold (Day1.findPassword Day1.betterCalc) input)]

main :: IO ()
main = do
  home <- getHomeDirectory
  let file = S.read . S.fromString_ $ home ++ inputPath
  defaultMain . tests $ file
