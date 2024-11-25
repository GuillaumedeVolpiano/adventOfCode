module Day10
  ( part1
  , part2
  ) where

import           Data.Either          (fromRight)
import           Data.Maybe           (fromJust)
import           Data.Sequence        as S (Seq, index, length)
import           Helpers.Parsers      (Parser, nums)
import           KnotHash             (fullHash, hash, initialCircle)
import           Numeric              (showHex)
import           Text.Megaparsec      (parse, sepBy)
import           Text.Megaparsec.Char (char)

parseInput :: Parser [Int]
parseInput = do
  sepBy (fromJust <$> nums) (char ',')

score :: (Int, Int, Seq Int) -> Int
score (curpos, _, circle) = a * b
  where
    origin = S.length circle - curpos
    a = index circle origin
    next = (origin + 1) `mod` S.length circle
    b = index circle next

part1 :: Bool -> String -> String
part1 test =
  show
    . score
    . hash (0, 0, initialCircle test)
    . fromRight []
    . parse parseInput ""

part2 :: Bool -> String -> String
part2 _ = concatMap (`showHex` "") . fullHash 
