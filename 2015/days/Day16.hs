{-# LANGUAGE TemplateHaskell #-}

module Day16
  ( part1
  , part2
  ) where

import           Data.ByteString           (ByteString)
import           Data.Void                 (Void)
import           Effectful.Dispatch.Static (Effect)
import           FlatParse.Basic           (anyAsciiDecimalInt, char, eof,
                                            isLatinLetter, optional_, runParser,
                                            satisfy, skipSatisfy, some, string,
                                            switch)
import           Helpers.Parsers.FlatParse (extract)
import qualified Helpers.Parsers.FlatParse as F (Parser)

type Parser = F.Parser Int

type PropParser = F.Parser Bool

eol :: F.Parser ()
eol = $(char '\n')

propParser :: Bool -> PropParser
propParser isPart2 = do
  prop <- some $ satisfy isLatinLetter
  $(string ": ")
  val <- anyAsciiDecimalInt
  optional_ $ satisfy (== ',') >> satisfy (== ' ')
  return . test isPart2 prop $ val

parseLine :: Bool -> Parser
parseLine isPart2 = do
  $(string "Sue ")
  sue <- anyAsciiDecimalInt
  $(string ": ")
  props <- some . propParser $ isPart2
  if and props
    then return sue
    else eol >> parseLine isPart2

test :: Bool -> String -> Int -> Bool
test _ "children"       = (== 3)
test True "cats"        = (> 7)
test _ "cats"           = (== 7)
test _ "samoyeds"       = (== 2)
test True "pomeranians" = (< 3)
test _ "pomeranians"    = (== 3)
test _ "akitas"         = (== 0)
test _ "vizslas"        = (== 0)
test True "goldfish"    = (< 5)
test _ "goldfish"       = (== 5)
test True "trees"       = (> 3)
test _ "trees"          = (== 3)
test _ "cars"           = (== 2)
test _ "perfumes"       = (== 1)

part1 :: Bool -> ByteString -> String
part1 _ = show . extract . runParser (parseLine False)

part2 :: Bool -> ByteString -> String
part2 _ = show . extract . runParser (parseLine True)
