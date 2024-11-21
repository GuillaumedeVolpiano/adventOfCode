module Day2
  ( part1
  , part2
  ) where

check :: [Int] -> Int
check l = maximum l - minimum l

divide :: [Int] -> Int
divide l =
  fst . head . filter (\(a, b) -> a /= 1 && b == 0) $ divMod <$> l <*> l

part1 :: Bool -> String -> String
part1 _ = show . sum . map (check . map read . words) . lines

part2 :: Bool -> String -> String
part2 _ = show . sum . map (divide . map read . words) . lines
