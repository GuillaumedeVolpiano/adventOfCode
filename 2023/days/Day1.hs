module Day1
  ( part1
  , part2
  ) where

import           Data.Char                    (digitToInt, isAlpha, isDigit)
import           Data.Either                  (fromRight)
import           Data.Maybe                   (catMaybes)
import           Data.Void                    (Void)
import           Text.Megaparsec              (Parsec, eof, lookAhead, manyTill,
                                               optional, parse, takeWhileP, try,
                                               (<|>))
import           Text.Megaparsec.Char         (char, digitChar, eol, lowerChar,
                                               string)

type Parser = Parsec Void String

parseInteger :: Parser (Maybe Int)
parseInteger = do
  _ <- takeWhileP Nothing (not . tester)
  Just . digitToInt <$> digitChar

parseOne :: Parser (Maybe Int)
parseOne = do
  _ <- takeWhileP Nothing (not . tester)
  _ <- string "on"
  lookAhead (char 'e')
  return (Just 1)

parseTwo :: Parser (Maybe Int)
parseTwo = do
  _ <- takeWhileP Nothing (not . tester)
  _ <- string "tw"
  lookAhead (char 'o')
  return (Just 2)

parseThree :: Parser (Maybe Int)
parseThree = do
  _ <- takeWhileP Nothing (not . tester)
  _ <- string "thre"
  lookAhead (char 'e')
  return (Just 3)

parseFour :: Parser (Maybe Int)
parseFour = do
  _ <- takeWhileP Nothing (not . tester)
  _ <- string "four"
  return (Just 4)

parseFive :: Parser (Maybe Int)
parseFive = do
  _ <- takeWhileP Nothing (not . tester)
  _ <- string "fiv"
  lookAhead (char 'e')
  return (Just 5)

parseSix :: Parser (Maybe Int)
parseSix = do
  _ <- takeWhileP Nothing (not . tester)
  _ <- string "six"
  return (Just 6)

parseSeven :: Parser (Maybe Int)
parseSeven = do
  _ <- takeWhileP Nothing (not . tester)
  _ <- string "eve"
  lookAhead (char 'n')
  return (Just 7)

parseEight :: Parser (Maybe Int)
parseEight = do
  _ <- takeWhileP Nothing (not . tester)
  _ <- string "eigh"
  lookAhead (char 't')
  return (Just 8)

parseNine :: Parser (Maybe Int)
parseNine = do
  _ <- takeWhileP Nothing (not . tester)
  _ <- string "nin"
  lookAhead (char 'e')
  return (Just 9)

tester :: Char -> Bool
tester c =
  isDigit c ||
  c == 'o' ||
  c == 't' || c == 'f' || c == 's' || c == 'e' || c == 'n' || c == '\n'

consume :: Parser (Maybe Int)
consume = do
  _ <- lowerChar
  return Nothing

simpleParser :: Parser (Maybe Int)
simpleParser = try parseInteger <|> consume

complexParser :: Parser (Maybe Int)
complexParser =
  try parseOne <|> try parseTwo <|> try parseThree <|> try parseFour <|>
  try parseFive <|>
  try parseSix <|>
  try parseSeven <|>
  try parseEight <|>
  try parseNine <|>
  try parseInteger <|>
  consume

parseLine :: Parser (Maybe Int) -> Parser [Int]
parseLine parser = catMaybes <$> manyTill parser eol

parseInput :: Parser (Maybe Int) -> Parser [[Int]]
parseInput parser = manyTill (parseLine parser) eof

firstLast :: [Int] -> Int
firstLast [] = 0
firstLast a  = 10 * head a + last a

part1 :: Bool -> String -> String
part1 _ =
  show . sum . map firstLast . fromRight [] . parse (parseInput simpleParser) ""

part2 :: Bool -> String -> String
part2 _ =
  show .
  sum . map firstLast . fromRight [] . parse (parseInput complexParser) ""
