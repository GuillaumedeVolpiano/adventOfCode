{-# LANGUAGE TemplateHaskell #-}

module Day8
  ( part1
  , part2
  ) where

import           Data.ByteString (ByteString)
import           Data.Void
import           FlatParse.Basic (Result (OK), anyChar, char, eof, runParser,
                                  switch, (<|>))
import qualified FlatParse.Basic as F (Parser)

type Parser = F.Parser Void Int

parseInput :: Parser -> Parser
parseInput parser = parseLine parser <|> (eof >> return 0)

parseLine :: Parser -> Parser
parseLine parser = parser <|> ($(char '\n') >> parseInput parser)

parseChar :: Parser
parseChar =
  $(switch
      [|case _ of
          "\""  -> (+ 1) <$> parseLine parseChar
          "\\x" -> anyChar >> anyChar >> (+ 3) <$> parseLine parseChar
          "\\"  -> anyChar >> (+ 1) <$> parseLine parseChar
          _     -> anyChar >> parseLine parseChar|])

parseChar' :: Parser
parseChar' =
  $(switch
      [|case _ of
          "\""  -> (+ 2) <$> parseLine parseChar'
          "\\x" -> anyChar >> anyChar >> (+ 1) <$> parseLine parseChar'
          "\\"  -> anyChar >> (+ 2) <$> parseLine parseChar'
          _     -> anyChar >> parseLine parseChar'|])

extract :: Result Void Int -> Int
extract (OK result _) = result

part1 :: Bool -> ByteString -> String
part1 _ = show . extract . runParser (parseInput parseChar)

part2 :: Bool -> ByteString -> String
part2 _ = show . extract . runParser (parseInput parseChar')
