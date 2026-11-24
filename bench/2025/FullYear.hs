{-# LANGUAGE ExistentialQuantification #-}
module Main where

import           Control.DeepSeq              (NFData, rnf)
import           Control.Exception            (evaluate)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS (readFile)
import           Data.Word                    (Word8)
import qualified Day1
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import           Helpers.Repository           (inputFile)
import qualified Streamly.Data.Stream         as S (fold, parse, unfold)
import           Streamly.Data.Stream         (Stream)
import qualified Streamly.External.ByteString as SBS (reader)
import           Test.Tasty.Bench             (Benchmark, bench, bgroup,
                                               defaultMain, env, nfAppIO)

-- Times every day of 2025, part by part, then the whole year in one go.
-- Reading the files is left out of the timings; parsing the stream is
-- included.
--
--   cabal bench full-year2025 --benchmark-options='--stdev 2'
--   cabal bench full-year2025 --benchmark-options='-p /Whole/'

-- | A part of a day: its name and what computes its answer.
data Part = forall a. NFData a => Part String (Stream IO Word8 -> IO a)

-- | The days, each with its parts. Day 10 solves both parts in one pass
-- over its machines, and day 12 has a single part.
days :: [(Int, [Part])]
days =
  [ (1, [Part "part 1" (S.fold (Day1.findPassword Day1.calc)), Part "part 2" (S.fold (Day1.findPassword Day1.betterCalc))])
  , (2, [Part "part 1" Day2.getCount, Part "part 2" Day2.getMoreCount])
  , (3, [Part "part 1" Day3.lowJoltage, Part "part 2" Day3.highJoltage])
  , (4, [Part "part 1" (\s -> Day4.mkMap s >>= Day4.countAccessible Day4.removeFree), Part "part 2" (\s -> Day4.mkMap s >>= Day4.countAccessible Day4.purge)])
  , (5, [Part "part 1" Day5.countFresh, Part "part 2" Day5.countAllFresh])
  , (6, [Part "part 1" (fmap Day6.getTotal . Day6.foldSheet), Part "part 2" (fmap Day6.getCorrectTotal . Day6.foldSheet)])
  , (7, [Part "part 1" Day7.countSplits, Part "part 2" Day7.countPaths])
  , (8, [Part "part 1" (Day8.findCircuits False), Part "part 2" Day8.findLastCircuits])
  , (9, [Part "part 1" Day9.getLargest, Part "part 2" Day9.getLargestValid])
  , (10, [Part "both parts" (fmap (either (\e -> error $ "Parser failed: " ++ show e) id) . S.parse Day10.parseMachines)])
  , (11, [Part "part 1" Day11.easyOut, Part "part 2" Day11.hardOut])
  , (12, [Part "part 1" Day12.validGrids])
  ]

-- | The input from memory.
stream :: ByteString -> Stream IO Word8
stream = S.unfold SBS.reader
{-# INLINE stream #-}

run :: ByteString -> Part -> IO ()
run input (Part _ solve) = solve (stream input) >>= evaluate . rnf

benchDay :: (Int, [Part]) -> IO Benchmark
benchDay (n, parts) = do
  file <- inputFile 2025 n
  pure . env (BS.readFile file) $ \input ->
    bgroup ("Day " ++ show n) [bench name $ nfAppIO (`run` part) input | part@(Part name _) <- parts]

wholeYear :: IO Benchmark
wholeYear = do
  files <- mapM (inputFile 2025 . fst) days
  pure . env (mapM BS.readFile files) $ \inputs ->
    bench "Whole year" $ nfAppIO (\is -> sequence_ [run i p | (i, (_, ps)) <- zip is days, p <- ps]) inputs

main :: IO ()
main = do
  perDay <- mapM benchDay days
  whole <- wholeYear
  defaultMain (perDay ++ [whole])
