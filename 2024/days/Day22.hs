module Day22
  ( part1
  , part2
  , getNSecrets
  , getNthSecret
  ) where

import           Data.Bits                  (shiftL, shiftR, xor, (.&.))
import           Data.Either                (fromRight)
import           Data.List                  (groupBy, nubBy, sortBy)
import           Data.Ord                   (comparing)
import           Data.Text                  (Text)
import           Helpers.Parsers.Text       (Parser)
import           Text.Megaparsec            (eof, manyTill, parse)
import           Text.Megaparsec.Char       (eol)
import           Text.Megaparsec.Char.Lexer (decimal)

parseInput :: Parser [Int]
parseInput = manyTill parseNumber eof

parseNumber :: Parser Int
parseNumber = do
  num <- decimal
  eol
  return num

getDiffs :: [Int] -> [Int]
getDiffs = zipWith (-) <*> tail

sequences :: [Int] -> [([Int], Int)]
sequences [w, x, y, z]       = []
sequences (a:xs@(b:c:d:e:_)) = ([a, b, c, d], e) : sequences xs

secret :: Int -> Int
secret a = prune sec3
  where
    sec1 = mix a . shiftL a $ 6
    sec2 = mix sec1 . flip shiftR 5 . prune $ sec1
    sec3 = mix sec2 . flip shiftL 11 . prune $ sec2

mix :: Int -> Int -> Int
mix a b = a `xor` b

prune :: Int -> Int
prune a = a .&. 16777215 -- 16777216 - 1

-- used in tests
getNSecrets :: Int -> Int -> [Int]
getNSecrets n = take n . iterate secret

getNthSecret :: Int -> Int -> Int
getNthSecret n = (!! n) . iterate secret

bestBananas :: [Int] -> Int
bestBananas =
  maximum
    . map (sum . map snd)
    . groupBy (\a b -> fst a == fst b)
    . sortBy (comparing fst)
    . concatMap (nubBy (\a b -> fst a == fst b) . sequences . getNSecrets 2001)

part1 :: Bool -> Text -> String
part1 _ =
  show
    . sum
    . map (getNthSecret 2000)
    . fromRight (error "parse failed")
    . parse parseInput "day22"

part2 :: Bool -> Text -> String
part2 _ =
  show
    . bestBananas
    . fromRight (error "parse failed")
    . parse parseInput "day22"
