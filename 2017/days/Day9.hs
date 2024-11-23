module Day9
  ( part1
  , part2
  ) where

import           Data.Either          (fromRight)
import           Helpers.Parsers      (Parser)
import           Text.Megaparsec      (manyTill, optional, parse, takeWhile1P,
                                       try, (<|>))
import           Text.Megaparsec.Char (char, printChar)

data Group
  = Group Int [Group]
  | Garbage Int
  deriving (Show)

parseGroup :: Int -> Parser Group
parseGroup depth = do
  char '{'
  contents <- manyTill (parseGroup (depth + 1) <|> parseGarbage) (char '}')
  optional . char $ ','
  return . Group depth $ contents

parseGarbage :: Parser Group
parseGarbage = do
  char '<'
  count <- manyTill (cancel <|> garb) (char '>')
  optional . char $ ','
  return . Garbage . sum $ count

cancel :: Parser Int
cancel = do
  char '!'
  printChar
  return 0

garb :: Parser Int
garb = do
  length <$> takeWhile1P Nothing (`notElem` "!>")

score1 :: Group -> Int
score1 (Garbage _) = 0
score1 (Group s g) = (+ s) . sum . map score1 $ g

score2 :: Group -> Int
score2 (Garbage g) = g
score2 (Group _ g) = sum . map score2 $ g

part1 :: Bool -> String -> String
part1 _ = show . score1 . fromRight (Garbage 0) . parse (parseGroup 1) ""

part2 :: Bool -> String -> String
part2 _ = show . score2 . fromRight (Garbage 0) . parse (parseGroup 1) ""
