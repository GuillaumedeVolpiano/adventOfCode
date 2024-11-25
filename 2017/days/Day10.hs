module Day10
  ( part1
  , part2
  ) where

import           Data.Bits            (xor)
import           Data.Char            (ord)
import           Data.Either          (fromRight)
import           Data.List            as L (foldl')
import           Data.Maybe           (fromJust)
import           Data.Sequence        as S (Seq, chunksOf, fromList, index,
                                            length, reverse, splitAt, (><))
import           Helpers.Parsers      (Parser, nums)
import           Numeric              (showHex)
import           Text.Megaparsec      (parse, sepBy)
import           Text.Megaparsec.Char (char)

initialCircle test
  | test = fromList [0 .. 4]
  | otherwise = fromList [0 .. 255]

extension = [17, 31, 73, 47, 23]

knot :: (Int, Int, Seq Int) -> Int -> (Int, Int, Seq Int)
knot (curpos, skip, circle) aLength =
  ( (curpos + skip + aLength) `mod` S.length circle
  , (skip + 1) `mod` S.length circle
  , untouched <> additional)
  where
    (toReverse, rest) = S.splitAt aLength circle
    int = rest <> S.reverse toReverse
    (additional, untouched) = S.splitAt skip int

hash :: (Int, Int, Seq Int) -> [Int] -> (Int, Int, Seq Int)
hash (pos, skip, circle) = foldl' knot (pos, skip, circle)

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

dense :: (Int, Int, Seq Int) -> String
dense (curPos, _, circle) = concatMap (hexify . densify) . chunksOf 16 $ rewound
  where
    origin = S.length circle - curPos
    (before, after) = S.splitAt origin circle
    rewound = after <> before
    densify = foldr xor 0
    hexify x
      | x < 16 = '0' : showHex x ""
      | otherwise = showHex x ""

part1 :: Bool -> String -> String
part1 test =
  show
    . score
    . hash (0, 0, initialCircle test)
    . fromRight []
    . parse parseInput ""

part2 :: Bool -> String -> String
part2 _ input =
  dense . (!! 64) . iterate (`hash` lengthList) $ (0, 0, initialCircle False)
  where
    lengthList = (++ extension) . map ord . init $ input
