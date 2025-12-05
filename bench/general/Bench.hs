{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as BS (readFile, unpack)
import qualified Day5
import qualified Streamly.Data.Stream as S (fromList, fold)
import           System.Directory     (getHomeDirectory)
import           Test.Tasty.Bench     (Benchmark, bench, defaultMain, env,
                                       whnfIO)
import Data.Function ((&))
import qualified Streamly.Data.Fold as F (drain)
import Streamly.Internal.Data.Fold (Step(Partial, Done))
import Data.Word8 (Word8, _lf)
import qualified Streamly.Internal.Data.Fold as F (foldt')

inputPath :: String
inputPath = "/github/adventOfCode/input/2025/day5.txt"

shortDrain :: Bool -> Word8 -> Step Bool ()
shortDrain b w
  | w == _lf && b = Done ()
  | w == _lf = Partial True
  | otherwise = Partial False

tests :: IO ByteString -> [Benchmark]
tests input =
  [ env (BS.unpack <$> input) $ \bs -> bench "Overhead" $ whnfIO $ S.fromList bs & S.fold F.drain
  , env (BS.unpack <$> input) $ \bs -> bench "Part 1" $ whnfIO $ S.fromList bs & Day5.countFresh
  , env (BS.unpack <$> input) $ \bs -> bench "Part 2 overhead" $ whnfIO $ S.fromList bs & S.fold (F.foldt' shortDrain (Partial True) (const ()))
  , env (BS.unpack <$> input) $ \bs -> bench "Part 2" $ whnfIO $ S.fromList bs & Day5.countAllFresh ]

main :: IO ()
main = do
  home <- getHomeDirectory
  let file = BS.readFile $ home ++ inputPath
  defaultMain . tests $ file
