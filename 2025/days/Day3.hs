{-# LANGUAGE BangPatterns #-}
module Day3
  ( part1
  , part2
  , lowJoltage
  , highJoltage
  ) where

import           Streamly.Data.Stream (Stream)
import           Data.Word (Word8)
import Data.Bits (shiftR, shiftL, (.&.))
import Data.Word8 (_lf)
import qualified Streamly.Data.Stream as S (fold)
import qualified Streamly.Data.Fold as F (foldl')

class State m where
  insert :: Word8 -> m -> m
  acc :: m -> m
  count :: m -> Word 

type FoldState = Word

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

unpackWord :: Word -> Word
unpackWord 0 = 0
unpackWord w = lo w + 10 * unpackWord (popRight w)
{-# INLINE unpackWord #-}

insertHFS :: Word -> HighFoldState -> HighFoldState
insertHFS w (HFS c v)
  | h == 0 = HFS c $ (v `shiftL` 4) +  w
  | otherwise = HFS c v'
  where
    v' = crawlVal (CS 0 r h pos) w
    pos = 44
    h = v `shiftR` pos
    r = v .&. (2^pos - 1 )
{-# INLINE insertHFS #-}

crawlVal :: CrawlState -> Word -> Word
crawlVal (CS seen _ cur 0) v
  | v > cur = seen' + v
  | otherwise = seen' + cur
  where
    seen' = seen `shiftL` 4 
crawlVal (CS seen toSee cur pos) v
  | new > cur = seen' +  (new `shiftL` pos) + (toSee' `shiftL` 4) + v
  | otherwise = crawlVal (CS seen'' toSee' new pos') v
  where
    seen' = seen `shiftL` (pos + 4)
    pos' = pos - 4
    new = toSee `shiftR` pos'
    toSee' = toSee .&. (2^pos' - 1)
    seen'' = seen `shiftL` 4 + cur

insertFS :: Word -> FoldState -> FoldState
insertFS !v !fs
  | v >= l || l > h = fs'
  | otherwise = fs
  where
    h = hi fs
    l = lo fs
    c = count fs
    fs'
      | l > h = (c `shiftL` 8) + (l `shiftL` 4) + v
      | l <= h = (c `shiftL` 8) + (h `shiftL` 4) + v
      | otherwise = undefined
{-# INLINE insertFS #-}

accHFS :: HighFoldState -> HighFoldState
accHFS (HFS !c !v) = HFS (c + unpackWord v) 0
{-# INLINE accHFS #-}

popRight :: Word -> Word
popRight w = w `shiftR` 4
{-# INLINE popRight #-}

accFS :: FoldState -> FoldState
accFS !fs = c' `shiftL` 8
  where
    h = hi fs
    l = lo fs
    c = count fs
    c' = c + 10 * h + l
{-# INLINE accFS #-}

countHFS :: HighFoldState -> Word
countHFS (HFS !c _ ) = c

countFS :: FoldState -> Word 
countFS = flip shiftR 8
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

-- | Fold function for part1: update FoldState based on current byte
countJoltage :: State m => m -> Word8 -> m
countJoltage fs !w
  | w == _lf = acc fs              -- end of line
  | isDigit w = insert w fs 
  | otherwise = undefined               -- invalid input
{-# INLINE countJoltage #-}

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
