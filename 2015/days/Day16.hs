{-# LANGUAGE TemplateHaskell #-}

module Day16
  ( part1
  , part2
  ) where

import           Control.Monad.Trans.Class (lift)
import           Data.ByteString           (ByteString)
import           Data.Void                 (Void)
import           FlatParse.Stateful        (Result (OK), anyAsciiDecimalInt,
                                            ask, char, eof, isLatinLetter,
                                            optional_, runParser, satisfy,
                                            skipSatisfy, some, string, switch)
import qualified FlatParse.Stateful        as F (Parser)

type Parser = F.Parser Bool Void Int

type PropParser = F.Parser Bool Void Bool

eol :: F.Parser Bool Void ()
eol = $(char '\n')

propParser :: PropParser
propParser = do
  prop <- some $ satisfy isLatinLetter
  $(string ": ")
  val <- anyAsciiDecimalInt
  optional_ $ satisfy (== ',') >> satisfy (== ' ')
  isPart2 <- ask
  return . test isPart2 prop $ val

parseLine :: Parser
parseLine = do
  $(string "Sue ")
  sue <- anyAsciiDecimalInt
  $(string ": ")
  props <- some propParser
  if and props
    then return sue
    else eol >> parseLine

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

extract :: Result Void Int -> Int
extract (OK result _ _) = result

part1 :: Bool -> ByteString -> String
part1 _ = show . extract . runParser parseLine False 0

part2 :: Bool -> ByteString -> String
part2 _ = show . extract . runParser parseLine True 0
