{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Day1
  ( part1
  , part2
  , findPassword
  , calc
  , betterCalc
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Word              (Word8)
import           Data.Word8             (_cr, _lf, _space)
import qualified Streamly.Data.Fold     as F (foldl')
import           Streamly.Data.Fold     (Fold)
import qualified Streamly.Data.Stream   as S (fold)
import           Streamly.Data.Stream   (Stream)

data FoldState = FS {
                      count :: {-# UNPACK #-} !Int
                    , pos :: {-# UNPACK #-} !Int
                    , dir :: {-# UNPACK #-} !Int
                    , val :: {-# UNPACK #-} !Int
                    }

findPassword :: MonadIO m => (FoldState -> FoldState) -> Fold m Word8 Int
findPassword c = count <$> F.foldl' (rotation c) (FS 0 50 0 0)

rotation :: (FoldState -> FoldState) -> FoldState -> Word8 -> FoldState
rotation c fs !w
  | w == 82 = fs {dir = 1}
  | w == 76 = fs {dir = -1}
  | w >= 48 && w <= 57 = fs {val = 10*val fs + fromIntegral (w - 48)}
  | w == _space || w == _lf || w == _cr = c fs
  | otherwise = error $ "unexpected byte" ++ show w
{-# INLINE rotation #-}

calc :: FoldState -> FoldState
calc fs = FS count' pos' 0 0
  where
    !v = dir fs * val fs
    !pos' = (pos fs + v) `mod` 100
    !count' = if pos' == 0 then count fs + 1
                           else count fs
{-# INLINE calc #-}

betterCalc :: FoldState -> FoldState
betterCalc fs = FS count' pos' 0 0
  where
   !v = dir fs * val fs
   !total = pos fs + v
   (!rotations, !pos') = total `divMod` 100
   delta
     | rotations <= 0 && pos' == 0 = 1
     | rotations < 0 && pos fs == 0 = - 1
     | otherwise = 0
   count' = count fs + abs rotations + delta
{-# INLINE betterCalc #-}

part1 :: Bool -> Stream IO Word8 -> IO ()
part1 _ s = S.fold (findPassword calc) s >>= print

part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ s = S.fold (findPassword betterCalc) s >>= print
