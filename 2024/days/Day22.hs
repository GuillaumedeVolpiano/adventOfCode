module Day22
  ( part1
  , part2
  , getNSecrets
  , getNthSecret
  , getDiffs
  ) where

import           Data.Bits                  (shiftL, shiftR, xor, (.&.))
import           Data.Either                (fromRight)
import           Data.IntMap.Strict         (IntMap, elems, insertWith)
import qualified Data.IntMap.Strict         as M (empty)
import           Data.IntSet                (IntSet, insert, member)
import qualified Data.IntSet                as S (empty)
import           Data.List                  (foldl', groupBy, nubBy, sortBy)
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

-- used in tests
getDiffs :: [Int] -> [Int]
getDiffs a = zipWith (-) (tail a) a

sequences ::
     Int -> IntSet -> (Int, Int, Int, Int, Int) -> IntMap Int -> IntMap Int
sequences counter seen (a, b, c, d, e) gainMap
  | counter == 2001 = gainMap
  | diffs `member` seen = sequences counter' seen (b, c, d, e', f) gainMap
  | otherwise = sequences counter' seen' (b, c, d, e', f) gainMap'
  where
    f = secret e
    e' = e `mod` 10
    counter' = counter + 1
    diffs =
      19 ^ 4 * (b - a) + 19 ^ 3 * (c - b) + 19 ^ 2 * (d - c) + 19 * (e' - d)
    seen' = insert diffs seen
    gainMap' = insertWith (+) diffs e' gainMap

initSequence :: Int -> (Int, Int, Int, Int, Int)
initSequence salt = (salt `mod` 10, b `mod` 10, c `mod` 10, d `mod` 10, e)
  where
    b = secret salt
    c = secret b
    d = secret c
    e = secret d

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

getNSecrets :: Int -> Int -> [Int]
getNSecrets n = take n . iterate secret

getNthSecret :: Int -> Int -> Int
getNthSecret n = (!! n) . iterate secret

bestBananas :: [Int] -> Int
bestBananas =
  maximum . elems . foldr (sequences 4 S.empty . initSequence) M.empty

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
