module Day7
  ( part1
  , part2
  ) where

import           Data.Either          (fromRight)
import           Helpers.Parsers      (Parser, decimal)
import           Text.Megaparsec      (eof, many, manyTill, optional, parse)
import           Text.Megaparsec.Char (eol, string)

import           Debug.Trace

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

canBeTrue :: Equation -> Bool
canBeTrue (Equation test (n:ns)) = fanOut test ns n
  where
    fanOut test [x] v    = v + x == test || v * x == test
    fanOut test (x:xs) v = fanOut test xs (x + v) || fanOut test xs (x * v)

canBeTrueConcat :: Equation -> Bool
canBeTrueConcat (Equation test (n:ns)) = fanOut test ns n
  where
    fanOut test [x] v = v + x == test || v * x == test || v ^|| x == test
    fanOut test (x:xs) v =
      fanOut test xs (x + v)
        || fanOut test xs (x * v)
        || fanOut test xs (v ^|| x)
    a ^|| b = pad a b + b
    pad a b = a * 10 ^ ((1 +) . floor . logBase 10 . fromIntegral $ b)

part1 :: Bool -> String -> String
part1 _ =
  show
    . foldr (\(Equation t _) -> (t +)) 0
    . filter canBeTrue
    . fromRight []
    . parse parseInput ""

part2 :: Bool -> String -> String
part2 _ =
  show
    . foldr (\(Equation t _) -> (t +)) 0
    . filter canBeTrueConcat
    . fromRight []
    . parse parseInput ""
