module Day1
  ( part1
  , part2
  ) where

import           Data.List       (sort, transpose)
import           Data.MultiSet   as MS (fromList, minView, null, occur)
import           Helpers.Parsers (numbers)

diff :: [[Int]] -> Int
diff [a, b] = sum . map abs . zipWith (-) a $ b

similarity :: [[Int]] -> Int
similarity [a, b] = sum . map appears $ a
  where
    -- pre-overengineering
    -- appears x = (* x) . length . filter (== x) $ b
    b' = fromList b
    appears x = (* x) . occur x $ b'

part1 :: Bool -> String -> String
part1 _ = show . diff . map sort . transpose . numbers

part2 :: Bool -> String -> String
part2 _ = show . similarity . transpose . numbers
