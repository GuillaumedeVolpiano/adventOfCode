module Main where

import qualified Data.ByteString              as BS (readFile)
import qualified Data.IntMap                  as IM (toList)
import           Days                         (days)
import           Helpers.Day                  (Day (Day))
import           Helpers.Input                (fromByteString)
import           Helpers.Repository           (inputFile)
import           System.Directory             (doesFileExist)
import           Test.Tasty.Bench             (Benchmark, bench, bgroup,
                                               defaultMain, nfAppIO)

-- Times both parts of every day of the table whose input has been
-- downloaded. Reading the file is left out of the timing; parsing the
-- input is included. Days without an input file are skipped.

year :: Int
year = 2026

benchDay :: Int -> Day -> IO [Benchmark]
benchDay n (Day solve1 _ solve2 _) = do
  file <- inputFile year n
  exists <- doesFileExist file
  if exists
    then do
      input <- fromByteString <$> BS.readFile file
      pure
        [ bgroup
            ("Day " ++ show n)
            [ bench "part 1" $ nfAppIO (solve1 False) input
            , bench "part 2" $ nfAppIO (solve2 False) input
            ]
        ]
    else pure []

main :: IO ()
main = do
  benchmarks <- concat <$> mapM (uncurry benchDay) (IM.toList days)
  defaultMain benchmarks
