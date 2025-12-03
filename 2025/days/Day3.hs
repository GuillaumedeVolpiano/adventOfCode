{-# LANGUAGE BangPatterns #-}
module Day3
  ( part1
  , part2
  , lowJoltage
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
  count :: Integral a => m -> a

type FoldState = Word

instance State Word where
  insert !w !fs = insertFS (digit w) fs
  acc = accFS
  count = fromIntegral . countFS

insertFS :: Word -> FoldState -> FoldState
insertFS !v !fs
  | v >= lo || lo > hi = fs'
  | otherwise = fs
  where
    hi = (fs `shiftR` 4) .&. 15
    lo = fs .&. 15
    c = count fs
    fs'
      | lo > hi = (c `shiftL` 8) + (lo `shiftL` 4) + v
      | lo <= hi = (c `shiftL` 8) + (hi `shiftL` 4) + v
      | otherwise = undefined
{-# INLINE insertFS #-}

accFS :: FoldState -> FoldState
accFS !fs = c' `shiftL` 8
  where
    hi = (fs `shiftR` 4) .&. 15
    lo = fs .&. 15
    c = count fs
    c' = c + 10*hi + lo
{-# INLINE accFS #-}

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

-- | Fold function for part1: update FoldState based on current byte
countJoltage :: State m => m -> Word8 -> m
countJoltage fs !w
  | w == _lf = acc fs              -- end of line
  | isDigit w = insert w fs 
  | otherwise = undefined               -- invalid input
{-# INLINE countJoltage #-}

totalJoltage :: (State m, Integral a) => m -> Stream IO Word8 -> IO a
totalJoltage m = fmap count . S.fold (F.foldl' countJoltage m)

lowJoltage :: Stream IO Word8 -> IO Int
lowJoltage = totalJoltage (0 :: Word)

part1 :: Bool -> Stream IO Word8 -> IO ()
part1 _ s  = lowJoltage s >>= print

part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ _ = print "Part 2"
