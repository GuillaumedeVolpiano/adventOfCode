{-# LANGUAGE TemplateHaskell #-}

module Day8
  ( part1
  , part2
  ) where

import           Data.ByteString           (ByteString)
import           Data.Void
import           FlatParse.Basic           (anyAsciiChar, char, eof, runParser,
                                            switch, (<|>))
import           Helpers.Parsers.FlatParse (extract)
import qualified Helpers.Parsers.FlatParse as F (Parser)

type Parser = F.Parser Int

parseInput :: Parser -> Parser
parseInput parser = parseLine parser <|> (eof >> return 0)

parseLine :: Parser -> Parser
parseLine parser = parser <|> ($(char '\n') >> parseInput parser)

parseChar :: Parser
parseChar =
  $(switch
      [|case _ of
          "\""  -> (+ 1) <$> parseLine parseChar
          "\\x" -> anyAsciiChar >> anyAsciiChar >> (+ 3) <$> parseLine parseChar
          "\\"  -> anyAsciiChar >> (+ 1) <$> parseLine parseChar
          _     -> anyAsciiChar >> parseLine parseChar|])

parseChar' :: Parser
parseChar' =
  $(switch
      [|case _ of
          "\"" -> (+ 2) <$> parseLine parseChar'
          "\\x" ->
            anyAsciiChar >> anyAsciiChar >> (+ 1) <$> parseLine parseChar'
          "\\" -> anyAsciiChar >> (+ 2) <$> parseLine parseChar'
          _ -> anyAsciiChar >> parseLine parseChar'|])

part1 :: Bool -> ByteString -> String
part1 _ = show . extract . runParser (parseInput parseChar)

part2 :: Bool -> ByteString -> String
part2 _ = show . extract . runParser (parseInput parseChar')
