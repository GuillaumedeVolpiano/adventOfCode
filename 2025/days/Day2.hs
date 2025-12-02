{-# LANGUAGE BangPatterns #-}
module Day2
  ( part1
  , part2
  , getCount
  , getMoreCount
  ) where

import           Data.Vector.Unboxed  (Vector)
import qualified Data.Vector.Unboxed  as V (generate, unsafeIndex)
import           Data.Word            (Word8)
import           Data.Word8           (_comma, _hyphen, _lf)
import qualified Streamly.Data.Fold   as F (foldl')
import qualified Streamly.Data.Stream as S (fold)
import           Streamly.Data.Stream (Stream)

data FoldState = FS {
                      lo      :: {-# UNPACK #-} !Int
                    , loleft  :: {-# UNPACK #-} !Int
                    , loright :: {-# UNPACK #-} !Int
                    , hi      :: {-# UNPACK #-} !Int
                    , count   :: {-# UNPACK #-} !Int
                    , onHigh  :: !Bool
                    }

data MoreFoldState = MFS {
                          mlo     :: {-# UNPACK #-} !Int
                        , mhi     :: {-# UNPACK #-} !Int
                        , mcount  ::{-# UNPACK #-}  !Int
                        , monHigh :: !Bool
                        }

-- we work under the assumption that we only get Int32s
pow10 :: Vector Int
pow10 = V.generate 11 (10^)

isDigit :: Word8 -> Bool
isDigit w = w - 48 <= 9

digit :: Word8 -> Int
digit w = fromIntegral (w - 48)

getCount :: Stream IO Word8 -> IO Int
getCount s = count <$> S.fold (F.foldl' countInvalid (FS 0 0 0 0 0 False)) s

countInvalid :: FoldState -> Word8 -> FoldState
countInvalid fs !w
  | w == _hyphen = treatLo fs
  | w == _comma = treatHi fs
  | w == _lf = treatHi fs
  | isDigit w = if onHigh fs then fs {hi = 10*hi fs + digit w}
                             else fs {lo = 10*lo fs + digit w}
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
    acc !v !c
      | v > lx = c
      | otherwise = acc (v + 1) $ c + (v * (pow10 `V.unsafeIndex` b)) + v
    count' = acc ln $ count fs
{-# INLINE treatHi #-}

splitInt :: Bool -> Int -> (Int, Int, Int)
splitInt isHi v
  | isHi && (even b && v == pow10 `V.unsafeIndex` b || odd b) =
      (pow10 `V.unsafeIndex` b' - 1, pow10 `V.unsafeIndex` b' - 1, b')
  | odd b = (pow10 `V.unsafeIndex` b', 0, b' + 1)
  | otherwise = (\(vl, vr) -> (vl, vr, b')) $ v `divMod` (pow10 `V.unsafeIndex` b')
  where
    b = fakeLog v
    b' = b `div` 2
{-# INLINE splitInt #-}

getMoreCount :: Stream IO Word8 -> IO Int
getMoreCount s = mcount <$> S.fold (F.foldl' countMoreInvalid (MFS 0 0 0 False)) s

countMoreInvalid :: MoreFoldState -> Word8 -> MoreFoldState
countMoreInvalid mfs !w
  | w == _hyphen = mfs {monHigh = True}
  | w == _comma || w == _lf = treatMoreHi mfs
  | isDigit w = if monHigh mfs then mfs { mhi = 10 * mhi mfs + digit w }
                               else mfs { mlo = 10 * mlo mfs + digit w }
  | otherwise = undefined

treatMoreHi :: MoreFoldState -> MoreFoldState
treatMoreHi mfs = mfs { mlo = 0, mhi = 0, monHigh = False, mcount = mcount' }
  where
    dilo = fakeLog $ mlo mfs
    dihi = fakeLog $ mhi mfs
    mcount'
      | dilo == dihi = sortAcc dilo ( mlo mfs ) ( mhi mfs ) ( mcount mfs )
      | otherwise = sortAcc dihi ( pow10 `V.unsafeIndex` dilo) ( mhi mfs ) 
          . sortAcc dilo ( mlo mfs ) ( (pow10 `V.unsafeIndex` dilo) - 1 ) $ mcount mfs
    sortAcc :: Int -> Int -> Int -> Int -> Int
    sortAcc dig l h c
      | dig == 1 = c
      | dig == 2 || dig == 3 || dig == 5  || dig == 7 = acc (splitMoreInt (l, h) dig 1) 1 dig c
      | dig == 4 = acc (splitMoreInt (l, h) 4 2) 2 2 c
      | dig == 9 = acc (splitMoreInt (l, h) 9 3) 3 3 c
      | dig == 8 = acc (splitMoreInt (l, h) 8 4) 4 2 c
      | dig == 6 = dec (splitMoreInt (l, h) 6 1) 6 . acc (splitMoreInt (l, h) 6 3) 3 2 . acc (splitMoreInt (l, h) 6 2) 2 3 $ c
      | dig == 10 = dec (splitMoreInt (l, h) 10 1) 10 . acc (splitMoreInt (l, h) 10 5) 5 2 . acc (splitMoreInt (l, h) 10 2) 2 5 $ c
      | otherwise = undefined
    acc :: (Int, Int) -> Int -> Int -> Int -> Int
    acc (l, h) splitDig splitOcc c
      | l > h || nv > mhi mfs = c
      | nv < mlo mfs = acc ( l + 1, h ) splitDig splitOcc c
      | otherwise = acc (l + 1, h) splitDig splitOcc $ c + nv
      where
        nv = buildVal l splitDig 0 splitOcc
    dec :: (Int, Int) -> Int -> Int -> Int
    dec (l, h) dig c
      | l > h || nv > mhi mfs = c
      | nv < mlo mfs = dec ( l + 1, h ) dig c
      | otherwise = dec (l + 1, h) dig $ c - nv
      where
        nv = buildVal l 1 0 dig

fakeLog :: Int -> Int
fakeLog !v
  | v <= 10         = 1
  | v <= 100        = 2
  | v <= 1000       = 3
  | v <= 10000      = 4
  | v <= 100000     = 5
  | v <= 1000000    = 6
  | v <= 10000000   = 7
  | v <= 100000000  = 8
  | v <= 1000000000 = 9
  | otherwise       = 10
{-# INLINE fakeLog #-}

splitMoreInt :: (Int, Int) -> Int -> Int -> (Int, Int)
splitMoreInt (!l, !h) !dig !curdiv = (l `div` (pow10 `V.unsafeIndex` dc) , h `div` (pow10 `V.unsafeIndex` dc))
  where
    dc = dig - curdiv
{-# INLINE splitMoreInt #-}

buildVal :: Int -> Int -> Int -> Int -> Int
buildVal _ _ !acc 0 = acc
buildVal !rep !split !acc !v = buildVal rep split ( acc * (pow10 `V.unsafeIndex` split) + rep ) ( v - 1 )
{-# INLINE buildVal #-}

part1 :: Bool -> Stream IO Word8 -> IO ()
part1 _ s = getCount s >>= print

part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ s = getMoreCount s >>= print
