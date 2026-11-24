module Main where

import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS (readFile)
import           Data.Word                    (Word8)
import qualified Day10
import           Helpers.Repository           (inputFile)
import qualified Streamly.Data.Stream         as S (parse, unfold)
import           Streamly.Data.Stream         (Stream)
import qualified Streamly.External.ByteString as SBS (reader)
import           Test.Tasty.Bench             (Benchmark, bench, defaultMain,
                                               env, nfAppIO)

-- Scratch benchmark for a day of 2025, whose solutions take a stream of
-- bytes: change the import, the day below and the benchmarks as needed.
--
--   cabal bench bench2025 --benchmark-options='--stdev 2'
--
-- To compare two versions of a solution, keep the first as a baseline:
--
--   cabal bench bench2025 --benchmark-options='--csv before.csv'
--   cabal bench bench2025 --benchmark-options='--baseline before.csv'

day :: Int
day = 10

-- | The input from memory: reading the file is left out of the timings.
stream :: ByteString -> Stream IO Word8
stream = S.unfold SBS.reader
{-# INLINE stream #-}

tests :: FilePath -> [Benchmark]
tests file =
  [ env (BS.readFile file) $ \input ->
      bench "Both parts with parsing" $ nfAppIO both input
  ]
  where
    both input =
      either (\e -> error $ "Parser failed: " ++ show e) id
        <$> S.parse Day10.parseMachines (stream input)

main :: IO ()
main = inputFile 2025 day >>= defaultMain . tests
