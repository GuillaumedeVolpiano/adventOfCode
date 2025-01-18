{-# LANGUAGE TupleSections #-}

module Day25
  ( part1
  , part2
  ) where

import           Control.Monad             (void)
import           Data.ByteString           (ByteString)
import           FlatParse.Basic           (anyAsciiDecimalInt, isDigit,
                                            runParser, skipSatisfy, some)
import           Helpers.Parsers.FlatParse (Parser, extract)

modulus = 33554393

operand = 252533

startPos = (1, 1)

startVal = 20151125

consume :: Parser ()
consume = void . some $ skipSatisfy (not . isDigit)

parseInput :: Parser (Int, Int)
parseInput =
  consume
    >> anyAsciiDecimalInt
    >>= \row -> consume >> (row, ) <$> anyAsciiDecimalInt

encode :: Int -> Int
encode = (`mod` modulus) . (* operand)

calculate :: (Int, Int) -> (Int, Int) -> Int -> Int
calculate goal cur@(row, col) curVal
  | cur == goal = curVal
  | row == 1 = calculate goal (col + 1, 1) . encode $ curVal
  | otherwise = calculate goal (row - 1, col + 1) . encode $ curVal

part1 :: Bool -> ByteString -> String
part1 _ =
  show . (\x -> calculate x startPos startVal) . extract . runParser parseInput

part2 :: Bool -> ByteString -> String
part2 _ _ = "Part 2"
