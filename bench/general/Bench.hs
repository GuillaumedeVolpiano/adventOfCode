{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as BS (readFile, unpack)
import qualified Day4
import qualified Streamly.Data.Stream as S (fromList)
import           System.Directory     (getHomeDirectory)
import           Test.Tasty.Bench     (Benchmark, bench, defaultMain, env,
                                       whnf)

inputPath :: String
inputPath = "/github/adventOfCode/input/2025/day4.txt"

tests :: ByteString -> [Benchmark]
tests input =
  [ env (Day4.mkMap . S.fromList . BS.unpack $ input) $ \rs -> bench "Part 1" $ whnf (Day4.countAccessible Day4.removeFree) rs
  , env (Day4.mkMap . S.fromList . BS.unpack $ input) $ \rs -> bench "Part 2" $ whnf (Day4.countAccessible Day4.purge) rs ]

main :: IO ()
main = do
  home <- getHomeDirectory
  file <- BS.readFile $ home ++ inputPath
  defaultMain . tests $ file
