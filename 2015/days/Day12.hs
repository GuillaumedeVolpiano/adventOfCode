{-# LANGUAGE TemplateHaskell #-}

module Day12
  ( part1
  , part2
  ) where

import           Data.ByteString           (ByteString)
import           FlatParse.Basic           (anyAsciiChar, anyAsciiDecimalInt,
                                            char, eof, isDigit, optional_,
                                            runParser, satisfy, skipSatisfy,
                                            switch, (<|>))
import           Helpers.Parsers.FlatParse (extract)
import qualified Helpers.Parsers.FlatParse as F (Parser)

type Parser = F.Parser Int

type ParserJ = F.Parser JSON

data JSON
  = Object [(String, JSON)]
  | Array [JSON]
  | String String
  | Number Int
  deriving (Eq)

parseNumbers :: Parser
parseNumbers =
  parseNegativeNumber
    <|> parseNumber
    <|> (skipSatisfy (not . isDigit) >> parseNumbers)
    <|> (eof >> return 0)

parseNegativeNumber :: Parser
parseNegativeNumber = do
  $(char '-')
  number <- negate <$> anyAsciiDecimalInt
  (+ number) <$> parseNumbers

parseNumber :: Parser
parseNumber = do
  number <- anyAsciiDecimalInt
  (number +) <$> parseNumbers

parseInput :: ParserJ
parseInput =
  $(switch
      [|case _ of
          "{"  -> Object <$> parseObject
          "["  -> Array <$> parseArray
          "\"" -> String <$> parseString
          "-"  -> Number . negate <$> anyAsciiDecimalInt
          _    -> Number <$> anyAsciiDecimalInt|])

parseArray :: F.Parser [JSON]
parseArray =
  $(switch
      [|case _ of
          "]" -> return []
          _   -> parseContents|])

parseContents :: F.Parser [JSON]
parseContents = do
  json <- parseInput
  optional_ . skipSatisfy $ (== ',')
  (json :) <$> parseArray

parseObject :: F.Parser [(String, JSON)]
parseObject =
  $(switch
      [|case _ of
          "}" -> return []
          _   -> parsePair|])

parsePair :: F.Parser [(String, JSON)]
parsePair = do
  $(char '"')
  key <- parseString
  $(char ':')
  json <- parseInput
  optional_ . skipSatisfy $ (== ',')
  ((key, json) :) <$> parseObject

parseString :: F.Parser String
parseString =
  (satisfy (== '"') >> return [])
    <|> (anyAsciiChar >>= \x -> (x :) <$> parseString)

evaluate :: JSON -> Int
evaluate (String _) = 0
evaluate (Number n) = n
evaluate (Array a) = sum . map evaluate $ a
evaluate (Object o)
  | "red" `elem` map fst o || String "red" `elem` map snd o = 0
  | otherwise = sum . map (evaluate . snd) $ o

part1 :: Bool -> ByteString -> String
part1 _ = show . extract . runParser parseNumbers

part2 :: Bool -> ByteString -> String
part2 _ = show . evaluate . extract . runParser parseInput
