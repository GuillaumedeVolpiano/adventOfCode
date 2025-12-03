{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as BS (readFile, unpack)
import           Data.Function        ((&))
import qualified Day3
import qualified Streamly.Data.Stream as S (fromList, fold)
import qualified Streamly.Data.Fold as F (drain)
import           System.Directory     (getHomeDirectory)
import           Test.Tasty.Bench     (Benchmark, bench, defaultMain, env,
                                       whnfIO)

inputPath :: String
inputPath = "/github/adventOfCode/input/2025/day3.txt"

tests :: IO ByteString -> [Benchmark]
tests input =
  [ env (BS.unpack <$> input) $ \bs -> bench "Construction overhead" $ whnfIO (S.fromList bs & S.fold F.drain)
  , env (BS.unpack <$> input) $ \bs -> bench "Part 1" $ whnfIO (S.fromList bs & Day3.lowJoltage )
  , env (BS.unpack <$> input) $ \bs -> bench "Part 2" $ whnfIO (S.fromList bs & Day3.highJoltage )]

main :: IO ()
main = do
  home <- getHomeDirectory
  let file = BS.readFile $ home ++ inputPath
  defaultMain . tests $ file
