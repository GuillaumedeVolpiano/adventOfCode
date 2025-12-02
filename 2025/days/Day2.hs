module Day2
  ( part1
  , part2
  , getCount
  ) where

import           Streamly.Data.Stream (Stream)
import qualified Streamly.Data.Stream as S (fold)
import           Data.Word (Word8)
import qualified Streamly.Data.Fold as F (foldl')
import Data.Word8 (_comma, _hyphen, _lf)

data FoldState = FS {
                      lo :: Int
                    ,  loleft :: Int
                    , loright :: Int
                    , hi :: Int
                    , count :: Int
                    , onHigh :: Bool
                    }

getCount :: Stream IO Word8 -> IO Int
getCount s = count <$> S.fold (F.foldl' countInvalid (FS 0 0 0 0 0 False)) s

countInvalid :: FoldState -> Word8 -> FoldState
countInvalid fs w
  | w == _hyphen = treatLo fs
  | w == _comma = treatHi fs
  | w == _lf = fs
  | w >= 48 && w <= 57 = if onHigh fs then fs {hi = 10*hi fs + fromIntegral (w - 48)}
                                      else fs {lo = 10*lo fs + fromIntegral (w - 48)}
  | otherwise = undefined
{-# INLINE countInvalid #-}

treatLo :: FoldState -> FoldState
treatLo fs = fs {lo = 0, loleft = ll, loright = lr, onHigh = True}
  where
    (ll, lr, _) = splitInt False $ lo fs
{-# INLINE treatLo #-}

treatHi :: FoldState -> FoldState
treatHi fs = fs { hi = 0 , onHigh = False, count = count'}
  where
    (hl, hr, b) = splitInt True $ hi fs
    ln = if loleft fs < loright fs then loleft fs + 1 else loleft fs
    lx = if hl <= hr then hl else hl - 1
    acc v c
      | v > lx = c
      | otherwise = acc (v + 1) $ c + (v * 10^b) + v
    count' = acc ln $ count fs
{-# INLINE treatHi #-}

splitInt :: Bool -> Int -> (Int, Int, Int)
splitInt isHi v 
  | isHi && (even b && v == 10^b || odd b) = (10^b' - 1, 10^b' - 1, b') 
  | odd b = (10^b', 0, b' + 1)
  | otherwise = (\(vl, vr) -> (vl, vr, b')) $ v `divMod` (10^b')
  where
    b = (ceiling :: Double -> Int) . logBase 10 . fromIntegral $ v
    b' = b `div` 2
{-# INLINE splitInt #-}

part1 :: Bool -> Stream IO Word8 -> IO ()
part1 _ s = getCount s >>= print

part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ _ = print "Part 2"
