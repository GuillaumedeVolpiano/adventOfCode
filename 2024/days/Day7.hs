module Day7
  ( part1
  , part2
  ) where

import           Control.Parallel.Strategies (parList, rseq, runEval)
import           Data.ByteString             (ByteString)
import           Data.Either                 (fromRight)
import           Helpers.Parsers.ByteString  (Parser, decimal, string)
import           Text.Megaparsec             (eof, many, manyTill, optional,
                                              parse)
import           Text.Megaparsec.Byte        (eol)

data Equation =
  Equation Test Numbers
  deriving (Show, Eq, Ord)

type Test = Int

type Numbers = [Int]

parseInput :: Parser [Equation]
parseInput = manyTill parseEquation eof

parseEquation :: Parser Equation
parseEquation = do
  test <- decimal
  string ": "
  numbers <- many decimal
  optional eol
  return . Equation test $ numbers

canBeTrue :: [Int -> Int -> Int] -> Equation -> Bool
canBeTrue ops (Equation test (n:ns)) = fanOut test ns n
  where
    fanOut test [x] v = any (\op -> test == op v x) ops
    fanOut test (x:xs) v
      | v >= test = False
      | otherwise = any (\op -> fanOut test xs (op v x)) ops

(^||) :: Int -> Int -> Int
a ^|| b = pad a b + b
  where
    pad a b = a * 10 ^ ((1 +) . floor . logBase 10 . fromIntegral $ b)

part1 :: Bool -> ByteString -> String
part1 _ =
  show
    . foldr (\(Equation t _) -> (t +)) 0
    . runEval
    . parList rseq
    . filter (canBeTrue [(+), (*)])
    . fromRight []
    . parse parseInput ""

part2 :: Bool -> ByteString -> String
part2 _ =
  show
    . foldr (\(Equation t _) -> (t +)) 0
    . filter (canBeTrue [(+), (*), (^||)])
    . runEval
    . parList rseq
    . fromRight []
    . parse parseInput ""
