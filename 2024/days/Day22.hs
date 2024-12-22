module Day22
  ( part1
  , part2
  , getNSecrets
  , getNthSecret
  , getDiffs
  ) where

import           Data.Bits                  (shiftL, shiftR, xor, (.&.))
import           Data.Either                (fromRight)
import           Data.List                  (foldl', groupBy, nubBy, sortBy)
import           Data.IntMap                   (IntMap, alter, elems)
import qualified Data.IntMap                   as M (empty)
import           Data.Ord                   (comparing)
import           Data.IntSet                   (IntSet, insert, member)
import qualified Data.IntSet                   as S (empty)
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
     [Int]
  -> IntSet
  -> IntMap Int
  -> IntMap Int
sequences [w, x, y, z] _ gainMap = gainMap
sequences (a:xs@(b:c:d:e:_)) seen gainMap
  | diffs `member` seen = sequences xs seen gainMap
  | otherwise = sequences xs seen' gainMap'
  where
    diffs = 19^4 * (b - a) + 19^3 * (c - b) + 19^2 * (d - c) + 19 * (e - d)
    seen' = insert diffs seen
    gainMap' = alter updateGain diffs gainMap
    updateGain Nothing  = Just e
    updateGain (Just s) = Just (s + e)

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
  maximum
    . elems
    . foldl' (\m p -> sequences p S.empty m) M.empty
    . map (map (`mod` 10) . getNSecrets 2001)

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
