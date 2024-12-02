module Day2
  ( part1
  , part2
  ) where

isSafe :: [Int] -> Bool
isSafe diffs =
  (all (> 0) diffs && not (any (> 3) diffs))
    || (all (< 0) diffs && not (any (< (-3)) diffs))

dampenedSafe :: [Int] -> Bool
dampenedSafe levels = dampen [] (head levels) (tail levels)

dampen :: [Int] -> Int -> [Int] -> Bool
dampen [] x xs@(y:ys)  = checkSafe xs || dampen [x] y ys
dampen xs x []         = checkSafe xs
dampen xs x xs'@(y:ys) = checkSafe (xs ++ xs') || dampen (xs ++ [x]) y ys

checkSafe :: [Int] -> Bool
checkSafe levels = isSafe . zipWith (-) (init levels) $ tail levels

part1 :: Bool -> String -> String
part1 _ = show . length . filter checkSafe . map (map read . words) . lines

part2 :: Bool -> String -> String
part2 _ = show . length . filter dampenedSafe . map (map read . words) . lines
