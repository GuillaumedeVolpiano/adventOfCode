module Day10
  ( part1
  , part2
  ) where

import           Data.ByteString           (ByteString)
import           Data.Char                 (digitToInt)
import           Data.List                 (span)
import           Data.Void                 (Void)
import           FlatParse.Basic           (isDigit, many, runParser, satisfy)
import           Helpers.Parsers.FlatParse (extract)
import qualified Helpers.Parsers.FlatParse as F (Parser)

type Parser = F.Parser [Int]

simpleParser :: Parser
simpleParser = many (digitToInt <$> satisfy isDigit)

runRound :: [Int] -> [Int]
runRound [] = []
runRound (x:xs) = (length x' + 1) : x : runRound xs'
  where
    (x', xs') = span (== x) xs

part1 :: Bool -> ByteString -> String
part1 _ =
  show . length . (!! 40) . iterate runRound . extract . runParser simpleParser

part2 :: Bool -> ByteString -> String
part2 _ =
  show . length . (!! 50) . iterate runRound . extract . runParser simpleParser
