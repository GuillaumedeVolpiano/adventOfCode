{-# LANGUAGE TemplateHaskell #-}

module Day17
  ( part1
  , part2
  ) where

import           Data.ByteString           (ByteString)
import           Data.List                 (group, sort)
import           Data.Maybe                (catMaybes)
import           FlatParse.Basic           (anyAsciiDecimalInt, char, runParser,
                                            some)
import           Helpers.Parsers.FlatParse (extract)
import qualified Helpers.Parsers.FlatParse as F (Parser)

type Parser = F.Parser [Int]

findSum :: Int -> [Int] -> Int
findSum _ [] = 0
findSum 0 _ = 1
findSum val nums@(n:ns)
  | val < 0 || sum nums < val = 0
  | sum nums == val = 1
  | n == val = 1 + findSum val ns
  | otherwise = findSum (val - n) ns + findSum val ns

findSumCounting :: Int -> [Int] -> [Maybe Int]
findSumCounting _ [] = [Nothing]
findSumCounting val nums@(n:ns)
  | val < 0 || sum nums < val = [Nothing]
  | sum nums == val = [Just . length $ nums]
  | n == val = Just 1 : findSumCounting val ns
  | otherwise =
    map ((+ 1) <$>) (findSumCounting (val - n) ns) ++ findSumCounting val ns

simpleParser :: Parser
simpleParser = some (anyAsciiDecimalInt >>= \x -> $(char '\n') >> return x)

part1 :: Bool -> ByteString -> String
part1 _ = show . findSum 150 . extract . runParser simpleParser

part2 :: Bool -> ByteString -> String
part2 _ =
  show
    . length
    . head
    . group
    . sort
    . catMaybes
    . findSumCounting 150
    . extract
    . runParser simpleParser
