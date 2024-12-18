module Day18
  ( part1
  , part2
  ) where

import           Data.Bits                  (shiftR, (.&.))
import           Data.Either                (fromRight)
import           Data.IntSet                (IntSet, empty, fromList, member,
                                             notMember)
import           Data.List                  (inits)
import           Data.List.Split            (splitOn)
import           Data.Maybe                 (fromJust)
import           Data.Text                  (Text, unpack)
import           Helpers.Parsers.Text       (Parser)
import           Helpers.Search.Int         (bfsSafeDist, dfs)
import           Text.Megaparsec            (eof, manyTill, parse)
import           Text.Megaparsec.Char       (char, eol)
import           Text.Megaparsec.Char.Lexer (decimal)

type Bytes = IntSet

parseInput :: Parser [Int]
parseInput = manyTill parseByte eof

parseByte :: Parser Int
parseByte = do
  x <- decimal
  char ','
  y <- decimal
  eol
  return (x + 128 * y)

origin = 0

goal test
  | test = 6 + 6 * 128
  | otherwise = 70 + 70 * 128

range test
  | test = (0, 6)
  | otherwise = (0, 70)

inRange :: (Int, Int) -> Int -> Bool
inRange (mini, maxi) int = mv >= mini && mV <= maxi
  where
    mv = min (int .&. 127) (int `shiftR` 7)
    mV = max (int .&. 127) (int `shiftR` 7)

intToPos :: Int -> String
intToPos int = show x ++ "," ++ show y
  where
    x = int .&. 127
    y = int `shiftR` 7

shortestPath :: Bool -> Bytes -> Int
shortestPath test bytes =
  fromJust . bfsSafeDist origin (neighbours test bytes) $ (== goal test)

hasPath :: Bool -> Bytes -> Bool
hasPath test bytes =
  member (goal test) . dfs [origin] (neighbours test bytes) $ empty

neighbours :: Bool -> Bytes -> Int -> [Int]
neighbours test bytes pos =
  filter (\p -> inRange (range test) p && p `notMember` bytes) . map (pos +)
    $ [-1, 1, -128, 128]

binary :: Bool -> Int -> Int -> [Int] -> Int
binary test low high bytes
  | low > length bytes = error "notFound"
  | hasPath test highBytes = binary test high (2 * high) bytes
  | low == high && not (hasPath test lowBytes) = bytes !! (low - 1)
  | low == high = error "not found"
  | high == low + 1 && hasPath test lowBytes = bytes !! (high - 1)
  | high == low + 1 = bytes !! (low - 1)
  | not (hasPath test lowBytes) = binary test low (div low 2) bytes
  | hasPath test midBytes = binary test mid high bytes
  | otherwise = binary test low mid bytes
  where
    highBytes = fromList . take high $ bytes
    lowBytes = fromList . take low $ bytes
    midBytes = fromList . take mid $ bytes
    mid = div (high + low) 2

part1 :: Bool -> Text -> String
part1 test =
  show
    . shortestPath test
    . fromList
    . take number
    . fromRight (error "parser failed")
    . parse parseInput "day18"
  where
    number
      | test = 12
      | otherwise = 1024

part2 :: Bool -> Text -> String
part2 test =
  intToPos
    . binary test 1024 2048
    . fromRight (error "parser failed")
    . parse parseInput "day18"
