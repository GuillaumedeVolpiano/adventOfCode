{-# LANGUAGE TupleSections #-}

module Day5
  ( part1
  , part2
  ) where

import           Data.List     as L (unfoldr)
import           Data.Sequence as S (Seq, adjust', fromList, lookup)

import           Debug.Trace

jump :: (Int -> Int) -> (Int, Seq Int) -> Maybe ((), (Int, Seq Int))
jump op (pointer, offsets) =
  ((), ) . (, newOffsets) . (+) pointer <$> newPointer
  where
    newPointer = S.lookup pointer offsets
    newOffsets = adjust' op pointer offsets

stranger :: Int -> Int
stranger things
  | things < 3 = things + 1
  | otherwise = things - 1

part1 :: Bool -> String -> String
part1 _ =
  show . length . unfoldr (jump (+ 1)) . (0, ) . fromList . map read . lines

part2 :: Bool -> String -> String
part2 _ =
  show . length . unfoldr (jump stranger) . (0, ) . fromList . map read . lines
