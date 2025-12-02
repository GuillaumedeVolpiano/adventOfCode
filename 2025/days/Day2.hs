{-# LANGUAGE BangPatterns #-}
module Day2
  ( part1
  , part2
  , getCount
  , getMoreCount
  ) where

import           Data.Word            (Word8)
import           Data.Word8           (_comma, _hyphen, _lf)
import qualified Streamly.Data.Fold   as F (foldl')
import qualified Streamly.Data.Stream as S (fold)
import           Streamly.Data.Stream (Stream)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS (insert, member)

data FoldState = FS {
                      lo      :: {-# UNPACK #-} !Int
                    , loleft :: {-# UNPACK #-} !Int
                    , loright :: {-# UNPACK #-} !Int
                    , hi      :: {-# UNPACK #-} !Int
                    , count   :: {-# UNPACK #-} !Int
                    , onHigh  :: {-# UNPACK #-} !Bool
                    }

data MoreFoldState = MFS {
                          mlo :: {-# UNPACK #-} !Int
                        , mhi :: {-# UNPACK #-} !Int
                        , mcount ::{-# UNPACK #-}  !Int
                        , monHigh :: {-# UNPACK #-} !Bool
                        }

getCount :: Stream IO Word8 -> IO Int
getCount s = count <$> S.fold (F.foldl' countInvalid (FS 0 0 0 0 0 False)) s

countInvalid :: FoldState -> Word8 -> FoldState
countInvalid fs !w
  | w == _hyphen = treatLo fs
  | w == _comma = treatHi fs
  | w == _lf = treatHi fs
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

getMoreCount :: Stream IO Word8 -> IO Int
getMoreCount s = mcount <$> S.fold (F.foldl' countMoreInvalid (MFS 0 0 0 False)) s

countMoreInvalid :: MoreFoldState -> Word8 -> MoreFoldState
countMoreInvalid mfs !w
  | w == _hyphen = mfs {monHigh = True}
  | w == _comma || w == _lf = treatMoreHi mfs
  | w >= 48 && w <= 57 = if monHigh mfs then mfs { mhi = 10 * mhi mfs + fromIntegral (w - 48) }
                                        else mfs { mlo = 10 * mlo mfs + fromIntegral (w - 48) }
  | otherwise = undefined
{-# INLINE countMoreInvalid #-}

treatMoreHi :: MoreFoldState -> MoreFoldState
treatMoreHi mfs = mfs { mlo = 0, mhi = 0, monHigh = False, mcount = snd mcount' }
  where
    dilo = countDigits $ mlo mfs
    dihi = countDigits $ mhi mfs
    mcount'
      | dilo == dihi = acc dilo ( maxDiv dilo ) ( mlo mfs ) ( mhi mfs ) 1 (mempty, mcount mfs)
      | otherwise = acc dihi ( maxDiv dihi ) ( 10^dilo) ( mhi mfs ) 1
          . acc dilo ( maxDiv dilo ) ( mlo mfs ) ( 10^dilo - 1 ) 1 $ (mempty, mcount mfs)
    acc :: Int -> Int -> Int -> Int -> Int -> (IntSet, Int) -> (IntSet, Int)
    acc dig mdiv l h curdiv c 
      | curdiv > mdiv = c 
      | dig == (curdiv ^ (2 :: Int)) = acc' (splitMoreInt (l, h) dig curdiv)
                  curdiv curdiv c
      | curdiv == 1 = acc dig mdiv l h 2 . acc' (splitMoreInt (l, h) dig 1) 1 dig $ c
      | dig `mod` curdiv == 0 = acc dig mdiv l h (curdiv + 1) . acc' (splitMoreInt (l, h) dig (dig `div` curdiv)) 
          (dig `div` curdiv) curdiv
          . acc' (splitMoreInt (l, h) dig curdiv) curdiv (dig `div` curdiv)$ c
      | otherwise = acc dig mdiv l h (curdiv + 1) c
    acc' :: (Int, Int) -> Int -> Int -> (IntSet, Int) -> (IntSet, Int)
    acc' _ _ 1 c = c
    acc' (l, h) splitDig splitOcc (seen, c) 
      | l > h = (seen, c)
      | nv `IS.member` seen = acc' (l + 1, h) splitDig splitOcc (seen, c)
      | nv < mlo mfs = acc' ( l + 1, h ) splitDig splitOcc (seen, c)
      | nv > mhi mfs = (seen, c)
      | otherwise = acc' (l + 1, h) splitDig splitOcc ( IS.insert nv seen, c + nv )
      where
        nv = buildVal l splitDig 0 splitOcc
{-# INLINE treatMoreHi #-}

maxDiv :: Int -> Int
maxDiv = (floor :: Double -> Int) . sqrt . fromIntegral
{-# INLINE maxDiv #-}

countDigits :: Int -> Int
countDigits !v
  | v == 10^b = b + 1
  | otherwise = b
  where
    b = (ceiling :: Double -> Int) . logBase 10 . fromIntegral $ v
{-# INLINE countDigits #-}

splitMoreInt :: (Int, Int) -> Int -> Int -> (Int, Int)
splitMoreInt (!l, !h) !dig !curdiv = (l `div` (10^dc) , h `div` (10^dc))
  where 
    dc = dig - curdiv
{-# INLINE splitMoreInt #-}

buildVal :: Int -> Int -> Int -> Int -> Int
buildVal _ _ !acc 0 = acc
buildVal !rep !split !acc !v = buildVal rep split ( acc * (10 ^ split) + rep ) ( v - 1 )
{-# INLINE buildVal #-}

part1 :: Bool -> Stream IO Word8 -> IO ()
part1 _ s = getCount s >>= print

part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ s = getMoreCount s >>= print
