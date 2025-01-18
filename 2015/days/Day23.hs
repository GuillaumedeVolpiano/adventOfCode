module Day23
  ( part1
  , part2
  ) where

import           Data.ByteString           (ByteString)
import           Data.List                 (unfoldr)
import           Helpers.Parsers.FlatParse (extract)
import qualified Helpers.Parsers.FlatParse as F (Parser)

-- The program sets a to 4591 if a is 0, 113383 if a is 1, then calculate the
-- collatz sequence of the number, using b to count the number of steps.
collatz :: Int -> Maybe (Int, Int)
collatz 1 = Nothing
collatz k
  | even k = Just (div k 2, div k 2)
  | otherwise = Just (3 * k + 1, 3 * k + 1)

part1 :: Bool -> ByteString -> String
part1 _ _ = show . length . unfoldr collatz $ 4591

part2 :: Bool -> ByteString -> String
part2 _ _ = show . length . unfoldr collatz $ 113383
