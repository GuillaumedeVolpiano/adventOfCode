module Day3
  ( part1
  , part2
  ) where

import           Control.Monad.State        (State, evalState, get, modify)
import           Data.Bifunctor             (second)
import           Data.Either                (fromRight)
import           Data.Text                  (Text, pack)
import           Data.Void                  (Void)
import           Text.Megaparsec            (ParsecT, eof, parse, runParserT,
                                             try, (<|>))
import           Text.Megaparsec.Char       (char, eol, printChar, string)
import           Text.Megaparsec.Char.Lexer (decimal)

type Parser = ParsecT Void Text (State (Bool, Bool))

parseInput :: Parser Int
parseInput =
  try parseMul
    <|> try
          (do
             string . pack $ "do()"
             modify (second . const $ True)
             parseInput)
    <|> try
          (do
             string . pack $ "don't()"
             modify (second . const $ False)
             parseInput)
    <|> (do
           printChar
           parseInput)
    <|> (do
           eol
           parseInput)
    <|> (do
           eof
           return 0)

parseMul :: Parser Int
parseMul = do
  (isPart1, isDo) <- get
  string . pack $ "mul("
  a <- decimal
  char ','
  b <- decimal
  char ')'
  if isPart1 || isDo
    then ((a * b) +) <$> parseInput
    else parseInput

part1 :: Bool -> Text -> String
part1 _ = show . fromRight 0 . flip evalState (True, True) . runParserT parseInput ""

part2 :: Bool -> Text -> String
part2 _ =
  show . fromRight 0 . flip evalState (False, True) . runParserT parseInput ""
