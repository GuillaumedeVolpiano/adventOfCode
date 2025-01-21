{-# LANGUAGE TemplateHaskell #-}

module Day1
  ( part1
  , part2
  ) where

import           Data.ByteString           (ByteString)
import           Data.Char                 (digitToInt)
import           FlatParse.Basic           (char, getPos, isDigit, runParser,
                                            satisfy, setPos, skip, skipSatisfy,
                                            some, switch, (<|>))
import           Helpers.Parsers.FlatParse (Parser, extract)

parseInteger :: Parser Int
parseInteger = digitToInt <$> satisfy isDigit

parseNumber :: Parser Int
parseNumber = do
  pos <- getPos
  (res, offset) <-
    $(switch
        [|case _ of
            "one"   -> pure (1, 2)
            "two"   -> pure (2, 2)
            "three" -> pure (3, 4)
            "four"  -> pure (4, 4)
            "five"  -> pure (5, 3)
            "six"   -> pure (6, 3)
            "seven" -> pure (7, 4)
            "eight" -> pure (8, 4)
            "nine"  -> pure (9, 3)|])
  setPos pos
  skip offset
  pure res

parseInput :: Parser [Int] -> Parser Int
parseInput parser =
  sum <$> some (parser >>= \ints -> pure ((head ints * 10) + last ints))

simpleParser :: Parser [Int]
simpleParser =
  ($(char '\n') >> pure [])
    <|> (parseInteger >>= \x -> (x :) <$> simpleParser)
    <|> (skip 1 >> simpleParser)

complexParser =
  ($(char '\n') >> pure [])
    <|> (parseInteger >>= \x -> (x :) <$> complexParser)
    <|> (parseNumber >>= \x -> (x :) <$> complexParser)
    <|> (skip 1 >> complexParser)

part1 :: Bool -> ByteString -> String
part1 _ = show . extract . runParser (parseInput simpleParser)

part2 :: Bool -> ByteString -> String
part2 _ = show . extract . runParser (parseInput complexParser)
