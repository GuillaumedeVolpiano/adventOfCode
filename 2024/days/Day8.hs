{-# LANGUAGE TupleSections #-}

module Day8
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed   (UArray, assocs, bounds, range)
import           Data.Char            (isAlphaNum)
import           Data.Function        (on)
import           Data.HashSet         (HashSet, empty, insert, size)
import           Data.List            (groupBy, sortBy, tails)
import           Data.Ord             (comparing)
import           Data.Ratio           ((%))
import           Data.Text            (Text)
import           Data.Tuple           (swap)
import           Helpers.Graph        (Pos)
import           Helpers.Parsers.Text (arrayFromText)
import           Linear.V2            (V2 (..))

type Frequency = [Pos]

type Grid = UArray Pos Char

type Rule = ([Pos] -> (Pos, Pos) -> [Pos])

findAntinodes :: [Pos] -> (Pos, Pos) -> [Pos]
findAntinodes pos (V2 x0 y0, V2 x1 y1) = filter (\x -> dist x && inLine x) pos
  where
    inLine (V2 x y)
      | y0 == y1 = y == y0
      | otherwise = y /= y0 && (x - x0) % (y - y0) == (x1 - x0) % (y1 - y0)
    dist (V2 x y) =
      (x - x0) ^ 2 + (y - y0) ^ 2 + (x - x1) ^ 2 + (y - y1) ^ 2
        == 5 * ((x0 - x1) ^ 2 + (y0 - y1) ^ 2)

findAligned :: [Pos] -> (Pos, Pos) -> [Pos]
findAligned pos (p1@(V2 x0 y0), V2 x1 y1) = p1 : filter inLine pos
  where
    inLine (V2 x y)
      | y0 == y1 = y == y0
      | otherwise = y /= y0 && (x - x0) % (y - y0) == (x1 - x0) % (y1 - y0)

frequencyAntinodes :: [Pos] -> Rule -> Frequency -> (HashSet Pos -> HashSet Pos)
frequencyAntinodes pos rule frequency = foldr (.) id antinodes
  where
    antinodes = zipWith freqFold <*> (tail . tails) $ frequency
    freqFold p = foldr ((.) . flip (foldr insert) . rule pos . (p, )) id

buildAntennas :: Grid -> [Frequency]
buildAntennas =
  map (foldr ((:) . snd) [])
    . groupBy ((==) `on` fst)
    . sortBy (comparing fst)
    . map swap
    . filter (isAlphaNum . snd)
    . assocs

findAllAntinodes :: Rule -> Grid -> Int
findAllAntinodes rule grid =
  size . foldr (frequencyAntinodes pos rule) empty $ antennas
  where
    antennas = buildAntennas grid
    pos = range . bounds $ grid

part1 :: Bool -> Text -> String
part1 _ = show . findAllAntinodes findAntinodes . arrayFromText

part2 :: Bool -> Text -> String
part2 _ = show . findAllAntinodes findAligned . arrayFromText
