{-# LANGUAGE TupleSections #-}

module Day6
  ( part1
  , part2
  ) where

import           Data.List     as L (length, unfoldr)
import           Data.Sequence as Sq (Seq, adjust', foldrWithIndex, fromList,
                                      length)
import           Data.Set      as S (Set, empty, insert, member)

redistribute :: Seq Int -> Seq Int
redistribute memory = distribute i b . adjust' (* 0) i $ memory
  where
    (i, b) = foldrWithIndex seqMax (0, 0) memory
    seqMax p v (ci, cm)
      | v >= cm = (p, v)
      | otherwise = (ci, cm)

distribute :: Int -> Int -> Seq Int -> Seq Int
distribute _ 0 memory = memory
distribute pointer counter memory =
  distribute newPointer (counter - 1) . adjust' (+ 1) newPointer $ memory
  where
    newPointer = (pointer + 1) `mod` Sq.length memory

debug1 :: (Seq Int, Set (Seq Int)) -> Maybe (Seq Int, (Seq Int, Set (Seq Int)))
debug1 (memory, seen)
  | memory `member` seen = Nothing
  | otherwise =
    Just (redistribute memory, (redistribute memory, insert memory seen))

debug2 :: Seq Int -> Int
debug2 memory = L.length . unfoldr debug1 $ (target, empty)
  where
    target = last . unfoldr debug1 $ (memory, empty)

part1 :: Bool -> String -> String
part1 _ =
  show . L.length . unfoldr debug1 . (, empty) . fromList . map read . words

part2 :: Bool -> String -> String
part2 _ = show . debug2 . fromList . map read . words
