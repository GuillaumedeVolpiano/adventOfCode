module Day20
  ( part1
  , part2
  ) where

import           Data.Either                (fromRight)
import           Data.List                  (sort)
import           Data.Text                  (Text)
import           Helpers.Parsers.Text       (Parser)
import           Text.Megaparsec            (eof, manyTill, parse)
import           Text.Megaparsec.Char       (char, eol)
import           Text.Megaparsec.Char.Lexer (decimal)

data Range =
  Range Low High
  deriving (Show, Eq)

instance Ord Range where
  compare (Range a b) (Range c d) = compare a c `mappend` compare b d

type Low = Int

type High = Int

parseInput :: Parser [Range]
parseInput = manyTill parseRange eof

parseRange :: Parser Range
parseRange = do
  a <- decimal
  char '-'
  b <- decimal
  eol
  return $ Range a b

merge :: Range -> [Range] -> [Range]
merge x [] = [x]
merge r1@(Range a b) rest@(r2@(Range c d):rest')
  | c > b + 1 = r1 : rest
  | b >= d = merge r1 rest'
  | otherwise = merge (Range a d) rest'

findFree :: [Range] -> Int
findFree [x]                            = 0
findFree (Range _ a:rest@(Range b _:_)) = b - a - 1 + findFree rest

part1 :: Bool -> Text -> String
part1 _ =
  show
    . (\(Range _ b) -> b + 1)
    . head
    . foldr merge []
    . sort
    . fromRight (error "parse failed")
    . parse parseInput "day20"

part2 :: Bool -> Text -> String
part2 _ =
  show
    . findFree
    . foldr merge []
    . sort
    . fromRight (error "parsefailed")
    . parse parseInput "20"
