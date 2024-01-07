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

parse :: Bool -> ExpParser
parse lp = parseExpr lp <* eol

parseExpr :: Bool -> ExpParser
parseExpr lp = do
  a <- parseVal lp
  parseRest lp a

parseRest :: Bool -> Expression -> ExpParser
parseRest lp a
  | lp =
    try
      (do v <- parseAdd lp a
          parseRest lp v) <|>
    parseMult lp a <|>
    return a
  | otherwise =
    try
      (do v <- try (parseAdd lp a) <|> parseMult lp a
          parseRest lp v) <|>
    return a

parseVal :: Bool -> ExpParser
parseVal lp = try (parseParens lp) <|> parseInt

parseParens :: Bool -> ExpParser
parseParens lp = char '(' *> parseExpr lp <* char ')'

parseInt :: ExpParser
parseInt = do
  t <- takeWhile1P Nothing isDigit
  return (Number $ read t)

parseAdd :: Bool -> Expression -> ExpParser
parseAdd lp exp = do
  void . string $ " + "
  Add exp <$> parseVal lp

parseMult :: Bool -> Expression -> ExpParser
parseMult lp exp
  | lp = do
    void . string $ " * "
    Mult exp <$> parseExpr lp
  | otherwise = do
    void . string $ " * "
    Mult exp <$> parseVal lp

evaluate :: Expression -> Int
evaluate (Number a) = a
evaluate (Mult a b) = evaluate a * evaluate b
evaluate (Add a b)  = evaluate a + evaluate b

part1 :: Bool -> String -> String
part1 _ = show . sum . map evaluate . parseByLine (parse False)

part2 :: Bool -> String -> String
part2 _ = show . sum . map evaluate . parseByLine (parse True)
