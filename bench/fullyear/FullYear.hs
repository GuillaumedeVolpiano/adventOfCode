module Main where

import           Data.ByteString      as B (ByteString, pack)
import qualified Data.ByteString      as B (readFile)
import           Data.ByteString.UTF8 (toString)
import           Data.Either          (fromRight)
import qualified Day1                 as D1
import qualified Day10                as D10
import qualified Day11                as D11
import qualified Day12                as D12
import qualified Day13                as D13
import qualified Day14                as D14
import qualified Day15                as D15
import qualified Day16                as D16
import qualified Day17                as D17
import qualified Day18                as D18
import qualified Day19                as D19
import qualified Day2                 as D2
import qualified Day20                as D20
import qualified Day21                as D21
import qualified Day22                as D22
import qualified Day23                as D23
import qualified Day24                as D24
import qualified Day25                as D25
import qualified Day3                 as D3
import qualified Day4                 as D4
import qualified Day5                 as D5
import qualified Day6                 as D6
import qualified Day7                 as D7
import qualified Day8                 as D8
import qualified Day9                 as D9
import           System.Directory     (getHomeDirectory)
import           Test.Tasty.Bench     (Benchmark, bcompare, bench, bgroup,
                                       defaultMain, nf)

input1 = "/adventOfCode/input/2023/day1.txt"

input2 = "/adventOfCode/input/2023/day2.txt"

input3 = "/adventOfCode/input/2023/day3.txt"

input4 = "/adventOfCode/input/2023/day4.txt"

input5 = "/adventOfCode/input/2023/day5.txt"

input6 = "/adventOfCode/input/2023/day6.txt"

input7 = "/adventOfCode/input/2023/day7.txt"

input8 = "/adventOfCode/input/2023/day8.txt"

input9 = "/adventOfCode/input/2023/day9.txt"

input10 = "/adventOfCode/input/2023/day10.txt"

input11 = "/adventOfCode/input/2023/day11.txt"

input12 = "/adventOfCode/input/2023/day12.txt"

input13 = "/adventOfCode/input/2023/day13.txt"

input14 = "/adventOfCode/input/2023/day14.txt"

input15 = "/adventOfCode/input/2023/day15.txt"

input16 = "/adventOfCode/input/2023/day16.txt"

input17 = "/adventOfCode/input/2023/day17.txt"

input18 = "/adventOfCode/input/2023/day18.txt"

input19 = "/adventOfCode/input/2023/day19.txt"

input20 = "/adventOfCode/input/2023/day20.txt"

input21 = "/adventOfCode/input/2023/day21.txt"

input22 = "/adventOfCode/input/2023/day22.txt"

input23 = "/adventOfCode/input/2023/day23.txt"

input24 = "/adventOfCode/input/2023/day24.txt"

input25 = "/adventOfCode/input/2023/day25.txt"

tests1 :: ByteString -> [Benchmark]
tests1 input =
  [ bench " Day 1, part 1" $ nf (D1.part1 False) . toString $ input
  , bench " Day 1, part 2" $ nf (D1.part2 False) . toString $ input
  ]

tests2 :: ByteString -> [Benchmark]
tests2 input =
  [ bench " Day 2, part 1" $ nf (D2.part1 False) . toString $ input
  , bench " Day 2, part 2" $ nf (D2.part2 False) . toString $ input
  ]

tests3 :: ByteString -> [Benchmark]
tests3 input =
  [ bench " Day 3, part 1" $ nf (D3.part1 False) . toString $ input
  , bench " Day 3, part 2" $ nf (D3.part2 False) . toString $ input
  ]

tests4 :: ByteString -> [Benchmark]
tests4 input =
  [ bench " Day 4, part 1" $ nf (D4.part1 False) . toString $ input
  , bench " Day 4, part 2" $ nf (D4.part2 False) . toString $ input
  ]

tests5 :: ByteString -> [Benchmark]
tests5 input =
  [ bench " Day 5, part 1" $ nf (D5.part1 False) . toString $ input
  , bench " Day 5, part 2" $ nf (D5.part2 False) . toString $ input
  ]

tests6 :: ByteString -> [Benchmark]
tests6 input =
  [ bench " Day 6, part 1" $ nf (D6.part1 False) . toString $ input
  , bench " Day 6, part 2" $ nf (D6.part2 False) . toString $ input
  ]

tests7 :: ByteString -> [Benchmark]
tests7 input =
  [ bench " Day 7, part 1" $ nf (D7.part1 False) . toString $ input
  , bench " Day 7, part 2" $ nf (D7.part2 False) . toString $ input
  ]

tests8 :: ByteString -> [Benchmark]
tests8 input =
  [ bench " Day 8, part 1" $ nf (D8.part1 False) . toString $ input
  , bench " Day 8, part 2" $ nf (D8.part2 False) . toString $ input
  ]

tests9 :: ByteString -> [Benchmark]
tests9 input =
  [ bench " Day 9, part 1" $ nf (D9.part1 False) . toString $ input
  , bench " Day 9, part 2" $ nf (D9.part2 False) . toString $ input
  ]

tests10 :: ByteString -> [Benchmark]
tests10 input =
  [ bench " Day 10, part 1" $ nf (D10.part1 False) . toString $ input
  , bench " Day 10, part 2" $ nf (D10.part2 False) . toString $ input
  ]

tests11 :: ByteString -> [Benchmark]
tests11 input =
  [ bench " Day 11, part 1" $ nf (D11.part1 False) . toString $ input
  , bench " Day 11, part 2" $ nf (D11.part2 False) . toString $ input
  ]

tests12 :: ByteString -> [Benchmark]
tests12 input =
  [ bench " Day 12, part 1" $ nf (D12.part1 False) . toString $ input
  , bench " Day 12, part 2" $ nf (D12.part2 False) . toString $ input
  ]

tests13 :: ByteString -> [Benchmark]
tests13 input =
  [ bench " Day 13, part 1" $ nf (D13.part1 False) . toString $ input
  , bench " Day 13, part 2" $ nf (D13.part2 False) . toString $ input
  ]

tests14 :: ByteString -> [Benchmark]
tests14 input =
  [ bench " Day 14, part 1" $ nf (D14.part1 False) . toString $ input
  , bench " Day 14, part 2" $ nf (D14.part2 False) . toString $ input
  ]

tests15 :: ByteString -> [Benchmark]
tests15 input =
  [ bench " Day 15, part 1" $ nf (D15.part1 False) . toString $ input
  , bench " Day 15, part 2" $ nf (D15.part2 False) . toString $ input
  ]

tests16 :: ByteString -> [Benchmark]
tests16 input =
  [ bench " Day 16, part 1" $ nf (D16.part1 False) . toString $ input
  , bench " Day 16, part 2" $ nf (D16.part2 False) . toString $ input
  ]

tests17 :: ByteString -> [Benchmark]
tests17 input =
  [ bench " Day 17, part 1" $ nf (D17.part1 False) . toString $ input
  , bench " Day 17, part 2" $ nf (D17.part2 False) . toString $ input
  ]

tests18 :: ByteString -> [Benchmark]
tests18 input =
  [ bench " Day 18, part 1" $ nf (D18.part1 False) . toString $ input
  , bench " Day 18, part 2" $ nf (D18.part2 False) . toString $ input
  ]

tests19 :: ByteString -> [Benchmark]
tests19 input =
  [ bench " Day 19, part 1" $ nf (D19.part1 False) . toString $ input
  , bench " Day 19, part 2" $ nf (D19.part2 False) . toString $ input
  ]

tests20 :: ByteString -> [Benchmark]
tests20 input =
  [ bench " Day 20, part 1" $ nf (D20.part1 False) . toString $ input
  , bench " Day 20, part 2" $ nf (D20.part2 False) . toString $ input
  ]

tests21 :: ByteString -> [Benchmark]
tests21 input =
  [ bench " Day 21, part 1" $ nf (D21.part1 False) . toString $ input
  , bench " Day 21, part 2" $ nf (D21.part2 False) . toString $ input
  ]

tests22 :: ByteString -> [Benchmark]
tests22 input =
  [ bench " Day 22, part 1" $ nf (D22.part1 False) . toString $ input
  , bench " Day 22, part 2" $ nf (D22.part2 False) . toString $ input
  ]

tests23 :: ByteString -> [Benchmark]
tests23 input =
  [ bench " Day 23, part 1" $ nf (D23.part1 False) . toString $ input
  , bench " Day 23, part 2" $ nf (D23.part2 False) . toString $ input
  ]

tests24 :: ByteString -> [Benchmark]
tests24 input =
  [ bench " Day 24, part 1" $ nf (D24.part1 False) . toString $ input
  , bench " Day 24, part 2" $ nf (D24.part2 False) . toString $ input
  ]

tests25 :: ByteString -> [Benchmark]
tests25 input =
  [ bench " Day 25, part 1" $ nf (D25.part1 False) . toString $ input
  , bench " Day 25, part 2" $ nf (D25.part2 False) . toString $ input
  ]

main = do
  home <- getHomeDirectory
  file1 <- B.readFile $ home ++ input1
  file2 <- B.readFile $ home ++ input2
  file3 <- B.readFile $ home ++ input3
  file4 <- B.readFile $ home ++ input4
  file5 <- B.readFile $ home ++ input5
  file6 <- B.readFile $ home ++ input6
  file7 <- B.readFile $ home ++ input7
  file8 <- B.readFile $ home ++ input8
  file9 <- B.readFile $ home ++ input9
  file10 <- B.readFile $ home ++ input10
  file11 <- B.readFile $ home ++ input11
  file12 <- B.readFile $ home ++ input12
  file13 <- B.readFile $ home ++ input13
  file14 <- B.readFile $ home ++ input14
  file15 <- B.readFile $ home ++ input15
  file16 <- B.readFile $ home ++ input16
  file17 <- B.readFile $ home ++ input17
  file18 <- B.readFile $ home ++ input18
  file19 <- B.readFile $ home ++ input19
  file20 <- B.readFile $ home ++ input20
  file21 <- B.readFile $ home ++ input21
  file22 <- B.readFile $ home ++ input22
  file23 <- B.readFile $ home ++ input23
  file24 <- B.readFile $ home ++ input24
  file25 <- B.readFile $ home ++ input25
  defaultMain
    $ concat
        [ tests1 file1
        , tests2 file2
        , tests3 file3
        , tests4 file4
        , tests5 file5
        , tests6 file6
        , tests7 file7
        , tests8 file8
        , tests9 file9
        , tests10 file10
        , tests11 file11
        , tests12 file12
        , tests13 file13
        , tests14 file14
        , tests15 file15
        , tests16 file16
        , tests17 file17
        , tests18 file18
        , tests19 file19
        , tests20 file20
        , tests21 file21
        , tests22 file22
        , tests23 file23
        , tests24 file24
        , tests25 file25
        ]
