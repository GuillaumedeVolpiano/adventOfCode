{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS (readFile, unpack)
import           Data.Function               ((&))
import           Data.Word8                  (Word8, _lf)
import qualified Day6
import qualified Streamly.Data.Fold          as F (drain)
import qualified Streamly.Data.Stream        as S (fold, fromList)
import           Streamly.Internal.Data.Fold (Step (Done, Partial))
import           System.Directory            (getHomeDirectory)
import           Test.Tasty.Bench            (Benchmark, bench, defaultMain,
                                              env, whnf, whnfIO)

inputPath :: String
inputPath = "/github/adventOfCode/input/2025/day6.txt"

shortDrain :: Bool -> Word8 -> Step Bool ()
shortDrain b w
  | w == _lf && b = Done ()
  | w == _lf = Partial True
  | otherwise = Partial False

tests :: ByteString -> IO ByteString -> [Benchmark]
tests input ioInput =
  [ env (Day6.foldSheet . S.fromList . BS.unpack $ input) $ \ws -> bench "Part 1" $ whnf Day6.getTotal ws
  , env (Day6.foldSheet . S.fromList . BS.unpack $ input) $ \ws -> bench "Part 2" $ whnf Day6.getCorrectTotal ws
  , env (BS.unpack <$> ioInput) $ \bs -> bench "Overhead" $ whnfIO $ S.fromList bs & S.fold F.drain
  , env (BS.unpack <$> ioInput) $ \bs -> bench "Part 1 with parsing" $ whnfIO $ Day6.getTotal <$> (S.fromList bs & Day6.foldSheet) 
  , env (BS.unpack <$> ioInput) $ \bs -> bench "Part 2 with parsing" $ whnfIO $ Day6.getCorrectTotal <$> (S.fromList bs & Day6.foldSheet)]

main :: IO ()
main = do
  home <- getHomeDirectory
  let file = BS.readFile $ home ++ inputPath
  file' <- file
  defaultMain . tests file' $ file
