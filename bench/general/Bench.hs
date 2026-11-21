module Main where

import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS (foldl', readFile)
import           Data.Word                    (Word8)
import qualified Day1
import           Helpers.Input                (bytes, fromByteString)
import           Helpers.Repository           (inputFile)
import qualified Streamly.Data.Fold           as F (foldl')
import qualified Streamly.Data.Stream         as S (fold)
import           Test.Tasty.Bench             (Benchmark, bcompare, bench,
                                               bgroup, defaultMain, env, nf,
                                               nfAppIO, nfIO)

-- Scratch benchmark for the day being optimised: change the import, the
-- day below and the benchmarks as needed.
--
--   cabal bench bench --benchmark-options='--stdev 2'
--
-- Read the allocations before the time: they do not vary from one run to
-- the next, and they tell whether the streams fused. A loop that fused
-- allocates next to nothing whatever the size of the input; one that did
-- not allocates some hundred bytes for each byte read.
--
-- To compare two versions of a solution, keep the first as a baseline:
--
--   cabal bench bench --benchmark-options='--csv before.csv'
--   cabal bench bench --benchmark-options='--baseline before.csv'

year, day :: Int
year = 2026
day = 1

tests :: FilePath -> [Benchmark]
tests file =
  [ -- The input is read before the timings, as the program does. nfAppIO
    -- rather than nfIO: a solution that does no real IO would otherwise be
    -- computed once and for all, outside what is timed.
    env (BS.readFile file) $ \input ->
      bgroup
        "Day"
        [ bench "Part 1" $ nfAppIO (Day1.solve1 False . fromByteString) input
        , bench "Part 2" $ nfAppIO (Day1.solve2 False . fromByteString) input
        ]
  , -- What cannot be beaten: going through the bytes once, without a
    -- stream and with one. Both should take the same time and allocate
    -- nothing. If the second does not, the streams do not fuse in this
    -- build, and the timings above measure that rather than the solution.
    env (BS.readFile file) $ \input ->
      bgroup
        "Floor"
        [ bench "bytes" $ nf walk input
        , bcompare "Floor.bytes" . bench "stream" . nfIO $
            S.fold (F.foldl' sumBytes 0) (bytes (fromByteString input))
        ]
  ]

walk :: ByteString -> Int
walk = BS.foldl' sumBytes 0
-- Inlined, the sum is computed once and for all, outside what is timed.
{-# NOINLINE walk #-}

sumBytes :: Int -> Word8 -> Int
sumBytes total byte = total + fromIntegral byte
{-# INLINE sumBytes #-}

main :: IO ()
main = inputFile year day >>= defaultMain . tests
