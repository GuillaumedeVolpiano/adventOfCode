module Day1
  ( part1
  , part2
  ) where

recCalcFuel :: Int -> Int
recCalcFuel mass
  | fuelMass <= 0 = 0
  | otherwise = fuelMass + recCalcFuel fuelMass
  where
    fuelMass = div mass 3 - 2

part1 :: Bool -> String -> String
part1 _ = show . sum . map ((-2 +) . flip div 3 . read) . lines

part2 :: Bool -> String -> String
part2 _ = show . sum . map (recCalcFuel . read) . lines
