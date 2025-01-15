{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
module Day19
  ( part1
  , part2
  ) where

import           Data.ByteString           (ByteString)
import           Data.ByteString.UTF8      (fromString)
import           Data.Map                  (Map, insert)
import           FlatParse.Basic           (isLatinLetter, many, satisfy,
                                            string, takeLine)
import qualified FlatParse.Basic           as B (char, runParser, (<|>))
import           Helpers.Parsers.FlatParse (extract, extractS)
import qualified Helpers.Parsers.FlatParse as F (Parser, ParserS)

type ParserMol = F.ParserS Formulas Int

type Parser = F.Parser (Formulas, Molecule)

type Formulas = Map String String
type Molecule = String

parseInput :: Parser
parseInput =  parseFormulas >>= \x -> takeLine >>= \y -> pure (x, y)

parseFormulas :: F.Parser Formulas
parseFormulas = ($(B.char '\n') >> return mempty) B.<|> parseLine

parseLine :: F.Parser Formulas
parseLine = do
  from <- many (satisfy isLatinLetter)
  $(string " => ")
  to <- many (satisfy isLatinLetter)
  $(B.char '\n')
  insert from to <$> parseFormulas

part1 :: Bool -> ByteString -> String
part1 _ =  show . extract . B.runParser parseInput

part2 :: Bool -> ByteString -> String
part2 _ _ = "Part 2"
