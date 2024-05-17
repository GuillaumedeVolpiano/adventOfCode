module Day12
  ( part1
  , part2
  ) where

import           Data.Set (Set, findMax, findMin, fromList, member)

type Pots = Set Int

type Growth = Set [Bool]

type State = (Pots, Growth)

parseInput :: String -> State
parseInput = (\(a:_:b) -> (pots a, growthList b)) . lines
  where
    pots =
      fromList .
      map fst . filter (\(_, c) -> c == '#') . zip [0 ..] . last . words
    growthList =
      fromList .
      map (map (== '#') . head) . filter ((== "#") . last) . map words

grow :: State -> State
grow (pots, growth) = (newPots, growth)
  where
    newPots =
      fromList
        [ x
        | x <- [(findMin pots - 2) .. (findMax pots + 2)]
        , flip member growth . map ((`member` pots) . (x +)) $ [(-2) .. 2]
        ]

longGrowth :: [Int] -> Int
longGrowth potsPattern =
  (potsPattern !! 100) +
  (50000000000 - 100) * ((potsPattern !! 100) - (potsPattern !! 99))

part1 :: Bool -> String -> String
part1 _ = show . sum . fst . (!! 20) . iterate grow . parseInput

part2 :: Bool -> String -> String
part2 _ =
  show . longGrowth . map (sum . fst) . take 101 . iterate grow . parseInput
