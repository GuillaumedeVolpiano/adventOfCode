{-# LANGUAGE BangPatterns #-}
-- Each input line is a bank of batteries represented as digits.
-- We must select exactly:
--  * 2 digits in Part 1 (“low joltage”)
--  * 12 digits in Part 2 (“high joltage”)
--
-- Digits must preserve their relative ordering from the input line.
-- For each line, the joltage is defined as the number formed by those digits.
-- The goal is to maximize this number per line, then sum across all lines.
--
-- This module parses the input as a stream of digit bytes and uses bit-level
-- packing to maintain digits efficiently in registers during the fold.

module Day3
  ( part1
  , part2
  , lowJoltage
  , highJoltage
  ) where

import           Streamly.Data.Stream (Stream)
import           Data.Word (Word8)
import Data.Bits (unsafeShiftR, unsafeShiftL, (.&.))
import Data.Word8 (_lf)
import qualified Streamly.Data.Stream as S (fold)
import qualified Streamly.Data.Fold as F (foldl')

-- | State m:
-- A generic "digit selection" fold state. Different implementations
-- store either 2 digits (FoldState/Word) or 12 digits (HighFoldState).
--
-- insert: Attempt to incorporate a new digit into the selection.
-- acc: Finalize and accumulate the current line (triggered by newline).
-- count: Extract total accumulated joltage so far.

class State m where
  insert :: Word8 -> m -> m
  acc :: m -> m
  count :: m -> Word 

-- | FoldState Word layout (for selecting 2 digits):
--   Bits [7..4] = high digit
--   Bits [3..0] = low digit
--   Upper bits  = accumulated total joltage across previous lines
--
-- `insertFS` keeps only the two most valuable digits in order.
-- `accFS` interprets them as a 2-digit number and adds to count.
type FoldState = Word

-- | HighFoldState:
--   c = accumulated joltage across completed lines
--   v = packed digit buffer for the current line
--
-- We pack up to 12 digits into v using 4 bits per digit.
data HighFoldState = HFS {-# UNPACK #-} !Word {-# UNPACK #-} !Word

data CrawlState = CS {-# UNPACK #-} !Word {-# UNPACK #-} !Word {-# UNPACK #-} !Word {-# UNPACK #-} !Int

instance State Word where
  insert !w !fs = insertFS (digit w) fs
  acc = accFS
  count = countFS

instance State HighFoldState where
  insert !w !fs = insertHFS (digit w) fs
  acc = accHFS
  count = countHFS

powTwo :: Int -> Word
powTwo p = 1 `unsafeShiftL` p
{-# INLINE powTwo #-}

-- | Convert a packed 4-bit-per-digit word (up to 12 digits)
-- into an actual decimal number while preserving digit order.
-- Tail-recursive through factor *= 10.
unpackWord :: Word -> Word -> Word
unpackWord _ 0 = 0
unpackWord !factor !w = lo w * factor + unpackWord (factor * 10) (popRight w)
{-# INLINE unpackWord #-}

-- | insertHFS: Insert a new digit into the packed buffer `v`.
-- Maintains:
--   • exactly 12 digits (discarding the smallest if > 12 appear)
--   • original left-to-right ordering
--   • lexicographically maximal number
--
-- Strategy:
--   If high bits empty, append easily.
--   Otherwise run a left-to-right crawl to find where the new digit
--   should slot in (if it improves the number).
insertHFS :: Word -> HighFoldState -> HighFoldState
insertHFS w (HFS c v)
  | h == 0 = HFS c $ (v `unsafeShiftL` 4) +  w
  | otherwise = HFS c v'
  where
    v' = crawlVal (CS 0 r h pos) w
    pos = 44
    h = v `unsafeShiftR` pos
    r = v .&. (powTwo pos - 1)
{-# INLINE insertHFS #-}

-- | crawlVal:
--   Walks the packed digits from left to right to find a position
--   where `v` can replace a weaker digit, while preserving slice order.
--
-- This implements the optimal subsequence-of-fixed-length selection
-- using only bit shifts (no lists/arrays).
crawlVal :: CrawlState -> Word -> Word
crawlVal (CS seen _ cur 0) v
  | v > cur = seen' + v
  | otherwise = seen' + cur
  where
    seen' = seen `unsafeShiftL` 4 
crawlVal (CS seen toSee cur pos) v
  | new > cur = seen' +  (new `unsafeShiftL` pos) + (toSee' `unsafeShiftL` 4) + v
  | otherwise = crawlVal (CS seen'' toSee' new pos') v
  where
    seen' = seen `unsafeShiftL` (pos + 4)
    pos' = pos - 4
    new = toSee `unsafeShiftR` pos'
    toSee' = toSee .&. (powTwo pos' - 1)
    seen'' = seen `unsafeShiftL` 4 + cur

insertFS :: Word -> FoldState -> FoldState
insertFS !v !fs
  | v >= l || l > h = fs'
  | otherwise = fs
  where
    h = hi fs
    l = lo fs
    c = count fs
    fs'
      | l > h = (c `unsafeShiftL` 8) + (l `unsafeShiftL` 4) + v
      | l <= h = (c `unsafeShiftL` 8) + (h `unsafeShiftL` 4) + v
      | otherwise = undefined
{-# INLINE insertFS #-}

accHFS :: HighFoldState -> HighFoldState
accHFS (HFS !c !v) = HFS (c + unpackWord 1 v) 0
{-# INLINE accHFS #-}

popRight :: Word -> Word
popRight w = w `unsafeShiftR` 4
{-# INLINE popRight #-}

accFS :: FoldState -> FoldState
accFS !fs = c' `unsafeShiftL` 8
  where
    h = hi fs
    l = lo fs
    c = count fs
    c' = c + 10 * h + l
{-# INLINE accFS #-}

countHFS :: HighFoldState -> Word
countHFS (HFS !c _ ) = c

countFS :: FoldState -> Word 
countFS = flip unsafeShiftR 8
{-# INLINE countFS #-}

-- | Check if a byte represents a decimal digit
isDigit :: Word8 -> Bool
isDigit w = w - 48 <= 9
{-# INLINE isDigit #-}

-- | Convert a digit byte to an Int
digit :: Word8 -> Word
digit w = fromIntegral (w - 48)
{-# INLINE digit #-}

hi :: Word -> Word
hi !w = popRight w .&. 15
{-# INLINE hi #-}

lo :: Word -> Word
lo !w = w .&. 15
{-# INLINE lo #-}

-- | Stream fold step:
--   On digit → try to insert into current state block
--   On newline → finalize the line and reset the block
countJoltage :: State m => m -> Word8 -> m
countJoltage fs !w
  | w == _lf = acc fs              -- end of line
  | isDigit w = insert w fs 
  | otherwise = undefined               -- invalid input
{-# INLINE countJoltage #-}

-- | Streamly effectful fold that scans all lines in the input stream,
-- using either 2-digit or 12-digit selection mode.
-- At the end, extracts the summed joltage.
totalJoltage :: State m => m -> Stream IO Word8 -> IO Word 
totalJoltage m = fmap count . S.fold (F.foldl' countJoltage m)

lowJoltage :: Stream IO Word8 -> IO Word
lowJoltage = totalJoltage (0 :: Word)

highJoltage :: Stream IO Word8 -> IO Word
highJoltage = totalJoltage (HFS 0 0)

part1 :: Bool -> Stream IO Word8 -> IO ()
part1 _ s  = lowJoltage s >>= print

part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ s = highJoltage s >>= print
