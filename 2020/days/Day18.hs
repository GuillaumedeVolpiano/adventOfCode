module Day18
  ( part1
  , part2
  ) where

import           Control.Monad        (void)
import           Data.Char            (isDigit)
import           Helpers.Parsers      (Parser, parseByLine)
import           Text.Megaparsec      (takeWhile1P, try, (<|>))
import           Text.Megaparsec.Char (char, eol, string)

data Expression
  = Number Int
  | Add Expression Expression
  | Mult Expression Expression

type ExpParser = Parser Expression

instance Show Expression where
  show (Number a) = show a
  show (Add a b)  = "(" ++ show a ++ " + " ++ show b ++ ")"
  show (Mult a b) = "(" ++ show a ++ "*" ++ show b ++ ")"

parsePrec :: ExpParser
parsePrec = parsePrecExpr <* eol

parse :: ExpParser
parse = parseExpr <* eol

parsePrecExpr :: ExpParser
parsePrecExpr = do
  a <- parsePrecVal
  parsePrecRest a

parseExpr :: ExpParser
parseExpr = do
  a <- parseVal
  parseRest a

parsePrecRest :: Expression -> ExpParser
parsePrecRest a =
  try
    (do v <- try (parsePrecAdd a)
        parsePrecRest v) <|>
  try (parsePrecMult a) <|>
  return a

parseRest :: Expression -> ExpParser
parseRest a =
  try
    (do v <- try (parseAdd a) <|> parseMult a
        parseRest v) <|>
  return a

parsePrecVal :: ExpParser
parsePrecVal = try parsePrecParens <|> parseInt

parseVal :: ExpParser
parseVal = try parseParens <|> parseInt

parsePrecParens :: ExpParser
parsePrecParens = char '(' *> parsePrecExpr <* char ')'

parseParens :: ExpParser
parseParens = char '(' *> parseExpr <* char ')'

parseInt :: ExpParser
parseInt = do
  t <- takeWhile1P Nothing isDigit
  return (Number $ read t)

parsePrecAdd :: Expression -> ExpParser
parsePrecAdd exp = do
  void . string $ " + "
  Add exp <$> parsePrecVal

parseAdd :: Expression -> ExpParser
parseAdd exp = do
  void . string $ " + "
  Add exp <$> parseVal

parsePrecMult :: Expression -> ExpParser
parsePrecMult exp = do
  void . string $ " * "
  Mult exp <$> parsePrecExpr

parseMult :: Expression -> ExpParser
parseMult exp = do
  void . string $ " * "
  Mult exp <$> parseVal

evaluate :: Expression -> Int
evaluate (Number a) = a
evaluate (Mult a b) = evaluate a * evaluate b
evaluate (Add a b)  = evaluate a + evaluate b

part1 :: Bool -> String -> String
part1 _ = show . sum . map evaluate . parseByLine parse

part2 :: Bool -> String -> String
part2 _ = show . sum . map evaluate . parseByLine parsePrec
