{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Day16
  ( part1
  , part2
  ) where

import           Control.Monad.Reader      (ReaderT, ask, runReaderT)
import           Control.Monad.Trans.Class (lift)
import           Data.ByteString           (ByteString)
import           Data.Void                 (Void)
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

propParser :: ReaderT Bool F.Parser Bool
propParser = do
  prop <- lift (some $ satisfy isLatinLetter)
  lift $(string ": ")
  val <- lift anyAsciiDecimalInt
  lift $ optional_ $ satisfy (== ',') >> satisfy (== ' ')
  isPart2 <- ask
  return . test isPart2 prop $ val

parseLine :: ReaderT Bool F.Parser Int
parseLine = do
  lift $(string "Sue ")
  sue <- lift anyAsciiDecimalInt
  lift $(string ": ")
  props <- some propParser
  if and props
    then return sue
    else lift eol >> parseLine

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
part1 _ = show . extract . runParser (runReaderT parseLine False)

part2 :: Bool -> ByteString -> String
part2 _ = show . extract . runParser (runReaderT parseLine True)
