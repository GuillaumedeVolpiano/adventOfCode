module Main where

import qualified Day1
import qualified Streamly.Data.Stream       as S (fold, fromList)
import           System.Directory           (getHomeDirectory)
import           Test.Tasty.Bench           (Benchmark, bench, defaultMain,
                                             whnfIO, env)
import qualified Data.ByteString as BS (readFile, unpack)
import Data.ByteString (ByteString)
import Data.Function ((&))

inputPath :: String
inputPath = "/github/adventOfCode/input/2025/day1.txt"

tests :: IO ByteString -> [Benchmark]
tests input =
  [ env input $ \bs -> bench "Part 1" $ whnfIO (S.fromList (BS.unpack bs) & S.fold (Day1.findPassword Day1.calc))
  , env input $ \bs -> bench "Part 2" $ whnfIO (S.fromList (BS.unpack bs) & S.fold (Day1.findPassword Day1.betterCalc))]

main :: IO ()
main = do
  home <- getHomeDirectory
  let file = BS.readFile $ home ++ inputPath
  defaultMain . tests $ file
