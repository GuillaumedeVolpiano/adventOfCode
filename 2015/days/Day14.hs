module Day14
  ( part1
  , part2
  ) where

import           Data.ByteString           (ByteString)
import           FlatParse.Basic           (anyAsciiDecimalInt, eof, isDigit,
                                            many, runParser, skipSatisfy, (<|>))
import           Helpers.Parsers.FlatParse (extract)
import qualified Helpers.Parsers.FlatParse as F (Parser)

consumer :: F.Parser [()]
consumer = many (skipSatisfy (not . isDigit))

simpleParser :: F.Parser [(Int, Int, Int)]
simpleParser =
  (eof >> return [])
    <|> (do
           consumer
           x <- anyAsciiDecimalInt
           consumer
           y <- anyAsciiDecimalInt
           consumer
           z <- anyAsciiDecimalInt
           consumer
           ((x, y, z) :) <$> simpleParser)

distance :: Int -> (Int, Int, Int) -> Int
distance t (speed, time, rest) = speed * t'
  where
    t' = fullLap * time + remainder
    modulus = time + rest
    fullLap = t `div` modulus
    remainder = min (t `mod` modulus) time

allocate :: [Int] -> [Int]
allocate dists =
  map
    (\x ->
       if x == md
         then 1
         else 0)
    dists
  where
    md = maximum dists

distribute :: [[Int]] -> [Int]
distribute = foldr (zipWith (+)) (repeat 0)

getDistances :: [Int] -> [(Int, Int, Int)] -> [[Int]]
getDistances times reindeers = map (\x -> map (distance x) reindeers) times

part1 :: Bool -> ByteString -> String
part1 _ =
  show . maximum . map (distance 2503) . extract . runParser simpleParser

part2 :: Bool -> ByteString -> String
part2 _ =
  show
    . maximum
    . distribute
    . map allocate
    . getDistances [1 .. 2503]
    . extract
    . runParser simpleParser
