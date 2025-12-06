{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS (readFile, unpack)
import           Data.Word8                  (Word8, _lf)
import qualified Day6
import qualified Streamly.Data.Stream        as S (fromList)
import           Streamly.Internal.Data.Fold (Step (Done, Partial))
import           System.Directory            (getHomeDirectory)
import           Test.Tasty.Bench            (Benchmark, bench,
                                              defaultMain, env, whnf)
import Data.Bifunctor (second)

inputPath :: String
inputPath = "/github/adventOfCode/input/2025/day6.txt"

shortDrain :: Bool -> Word8 -> Step Bool ()
shortDrain b w
  | w == _lf && b = Done ()
  | w == _lf = Partial True
  | otherwise = Partial False

tests :: ByteString -> [Benchmark]
tests input =
  [ env (Day6.getWrongWorksheet . S.fromList . BS.unpack $ input) $ \ws -> bench "Part 1" $ whnf (uncurry Day6.getTotal . second Day6.buildWrongWorksheet) ws
  , env (Day6.getCorrectWorksheet . S.fromList . BS.unpack $ input) $ \ws -> bench "Part 2" $ whnf (uncurry Day6.getCorrectTotal) ws]

main :: IO ()
main = do
  home <- getHomeDirectory
  file <- BS.readFile $ home ++ inputPath
  defaultMain . tests $ file
