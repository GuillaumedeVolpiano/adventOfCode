

-- |
-- Module      : Day4
-- Description : Grid accessibility analysis using mutable vectors and IntSet frontier propagation.
--
-- This module provides two functions ('part1' and 'part2') for solving the
-- Advent of Code Day 4 problem, measuring accessibility in a grid structure.
-- The grid is represented as a flat mutable vector, and frontier pruning is
-- used to efficiently compute reachable or removable cells.
module Day4
  ( part1
  , part2
  , mkMap
  , countAccessible
  , removeFree
  , purge
  ) where

import           Control.DeepSeq             (NFData, rnf)
import           Control.Monad               (foldM)
import           Data.Bifunctor              (first)
import           Data.Bits                   (shiftL, (.|.))
import           Data.IntSet                 (IntSet)
import qualified Data.IntSet                 as IS (insert, size)
import           Data.Sequence               (Seq ((:|>)), ViewL (EmptyL, (:<)))
import qualified Data.Sequence               as Seq (viewl)
import           Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as MV (new, unsafeRead,
                                                    unsafeWrite)
import           Data.Word                   (Word8)
import qualified Streamly.Data.Fold          as F (foldlM')
import           Streamly.Data.Fold          (Fold)
import qualified Streamly.Data.Stream        as S (fold)
import           Streamly.Data.Stream        (Stream)

-- | State during processing:
--
-- * A mutable occupancy grid
-- * A frontier queue of positions to check
-- * A set of confirmed \"freed\" positions
data ForkRolls = FR !(IOVector Bool) !(Seq Int) !IntSet

-- | For benchmarks
instance NFData ForkRolls where
  rnf (FR _ sq st) = rnf sq `seq` rnf st

-- | Iteratively prune the frontier:
--
-- * If a cell has fewer than 4 occupied neighbors, it becomes \"free\"
-- * The freed cell is marked False and added to the free set
-- * Newly affected neighbors are appended to the frontier
--
-- Continues until no more cells can be removed.
purge :: ForkRolls -> IO IntSet
purge (FR rs rseq fs) = case Seq.viewl rseq of
                          EmptyL -> pure fs
                          (next :< rest) -> do
                            stillOccupied <- MV.unsafeRead rs next :: IO Bool
                            if stillOccupied then do
                                             let checkFree :: Int -> [Int] -> IO (Maybe (Seq Int, IntSet))
                                                 checkFree 4 _ = pure Nothing
                                                 checkFree _ [] = do
                                                   MV.unsafeWrite rs next False
                                                   pure . Just $ (rseq, IS.insert next fs)
                                                 checkFree c (x:xs)
                                                  | next + x < 0 = checkFree c xs
                                                  | otherwise = do
                                                      occ <- MV.unsafeRead rs (next + x)
                                                      if occ then fmap (first (:|> (next +  x)))
                                                                    <$> checkFree (c + 1) xs
                                                             else checkFree c xs
                                             res <- checkFree 0 [-255, -256, -257, -1, 1, 255, 256, 257]
                                             case res of
                                               Nothing -> purge (FR rs rest fs)
                                               Just (rseq', fs') -> purge (FR rs rseq' fs')
                                             else purge (FR rs rest fs)

-- | Compute the set of accessible cells without modifying the grid in place.
--
-- This performs the same neighbor counting as 'purge', but does not propagate
-- frontier changes.
removeFree :: ForkRolls -> IO IntSet
removeFree (FR rs rseq freeSet) = do
  let isNotFree s p = insertFree p 0 s [-255, -256, -257, -1, 1, 255, 256, 257]
      insertFree :: Int -> Int -> IntSet -> [Int] -> IO IntSet
      insertFree _ 4 s _ = pure s
      insertFree p _ s [] = pure $ IS.insert p s
      insertFree p c s (x:xs)
        | p + x >= 0 = do
            occ <- MV.unsafeRead rs (p + x)
            if occ then insertFree p (c + 1) s xs
                   else insertFree p c s xs
        | otherwise = insertFree p c s xs
  foldM isNotFree freeSet rseq

-- | Count the number of accessible cells given a strategy
-- such as 'purge' or 'removeFree'.
countAccessible :: (ForkRolls -> IO IntSet) -> ForkRolls -> IO Int
countAccessible f = fmap IS.size . f

-- | Internal stream reader converting ByteString map input into a populated 'ForkRolls'.
--
-- `'@'` (64) becomes an occupied cell.
-- `'.'` (46) becomes empty.
-- Newlines adjust indexing by a full row width.
mapReader :: IO (Int, ForkRolls) -> Fold IO Word8 ForkRolls
mapReader = fmap snd . F.foldlM' folder
  where
    folder :: (Int, ForkRolls) -> Word8 -> IO (Int, ForkRolls)
    folder (pos, FR rs rseq freeSet) w = case w of
                            64 -> do
                              MV.unsafeWrite rs pos True
                              pure (pos + 1, FR rs (rseq :|> pos) freeSet)
                            46 -> pure (pos + 1, FR rs rseq freeSet)
                            10 -> pure ((pos .|. 255) + 1, FR rs rseq freeSet)
                            _ -> error $ "unexpectd bit" ++ show w

-- | Construct the mutable grid and frontier queue from stream input.
mkMap :: Stream IO Word8 -> IO ForkRolls
mkMap s = do
  mv <- MV.new (1 `shiftL` 16)
  S.fold (mapReader $ pure (0, FR mv mempty mempty)) s

-- | Solution for Part 1 (static accessibility).
--
-- Ignores frontier pruning and evaluates all candidates.
part1 :: Bool -> Stream IO Word8 -> IO ()
part1 _ s = mkMap s >>= countAccessible removeFree >>= print

-- | Solution for Part 2 (dynamic frontier pruning).
--
-- Much faster thanks to cascading updates.
part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ s = mkMap s >>= countAccessible purge >>= print
