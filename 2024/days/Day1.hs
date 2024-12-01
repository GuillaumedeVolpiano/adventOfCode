module Day1
  ( part1
  , part2
  ) where

import           Data.List       (sort, transpose)
import           Data.MultiSet   (fromList, occur)
import           Helpers.Parsers (numbers)

diff :: [[Int]] -> [Int]
diff [a, b] = map abs . zipWith (-) a $ b

similarity :: [[Int]] -> [Int]
similarity [a, b] = map appears a
  where
    b' = fromList b
    appears x = (* x) . occur x $ b'

-- pre-overengineering
-- appears x = (* x) . length . filter (== x) $ b
part1 :: Bool -> String -> String
part1 _ = show . sum . diff . map sort . transpose . numbers

part2 :: Bool -> String -> String
part2 _ = show . sum . similarity . transpose . numbers
