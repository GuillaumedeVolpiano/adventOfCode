{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as BS (readFile, unpack)
import           Data.Function                 ((&))
import           Data.Word8                    (Word8, _lf)
import qualified Day9
import qualified Streamly.Data.Fold            as F (drain)
import qualified Streamly.Data.Stream          as S (fold, fromList)
import           Streamly.Internal.Data.Fold   (Step (Done, Partial))
import           System.Directory              (getHomeDirectory)
import           Test.Tasty.Bench              (Benchmark, bench, defaultMain,
                                                env, whnfIO)

inputPath :: String
inputPath = "/github/adventOfCode/input/2025/day9.txt"

shortDrain :: Bool -> Word8 -> Step Bool ()
shortDrain b w
  | w == _lf && b = Done ()
  | w == _lf = Partial True
  | otherwise = Partial False

tests :: IO ByteString -> [Benchmark]
tests ioInput =
  [ env (BS.unpack <$> ioInput) $ \bs -> bench "Overhead" $ whnfIO $ S.fromList bs & S.fold F.drain
  , env (BS.unpack <$> ioInput) $ \bs -> bench "Part 1 with parsing" $ whnfIO $ S.fromList bs & Day9.getLargest
--  , env (BS.unpack <$> ioInput) $ \bs -> bench "Part 2 with parsing" $ whnfIO $ S.fromList bs
--          & Day8.findLastCircuits
  ]

main :: IO ()
main = do
  home <- getHomeDirectory
  let file = BS.readFile $ home ++ inputPath
  defaultMain $ tests file
