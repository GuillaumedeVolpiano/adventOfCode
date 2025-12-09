{-# LANGUAGE LambdaCase #-}

module Day9
  ( part1
  , part2
  , getLargest
  , getLargestValid
  ) where

import           Data.IORef                   (IORef)
import qualified Data.IORef                   as R (modifyIORef', newIORef,
                                                    readIORef, writeIORef)
import           Data.Maybe                   (fromMaybe)
import           Data.Ord                     (Down (Down), comparing)
import           Data.Vector.Unboxed.Mutable  (IOVector, Unbox)
import qualified Data.Vector.Unboxed.Mutable  as MV (unsafeNew, unsafeRead,
                                                     unsafeWrite)
import           Data.Word                    (Word8)
import           Data.Word8                   (_comma, _lf)
import           Helpers.General.Streamly     (digit, isDigit)
import           Helpers.Heap.Unboxed.Mutable (Heap, newHeap, popMin, push)
import qualified Streamly.Data.Fold           as F (foldlM')
import qualified Streamly.Data.Stream         as S (fold)
import           Streamly.Data.Stream         (Stream)
-- import Control.Monad (when)

type Rect = (Coords, Coords)

type Areas = Heap (Int, Rect)

type Coords = (Int, Int)

data FoldState = FS !Areas !(IORef [Coords]) !(IORef [Int]) !(RingBuffer Coords)

data RingBuffer a = RB (IOVector a) !(IORef Int) !(IORef Int) !(IORef Int) !(IORef Int)

newBuffer :: (Unbox a) => Int -> IO (RingBuffer a)
newBuffer sz = do
  rb <- MV.unsafeNew sz
  start <-  R.newIORef 0
  end <- R.newIORef 0
  pos <- R.newIORef 0
  rsz <- R.newIORef sz
  pure $ RB rb start end pos rsz

put :: (Unbox a) => RingBuffer a -> a -> IO ()
put (RB rb start end _ sz) v = do
  st <- R.readIORef start
  e <- R.readIORef end
  s <- R.readIORef sz
  let ne = (e + 1) `mod` s
  if ne == st then error "Full buffer"
                   else do
    MV.unsafeWrite rb e v
    R.writeIORef end ne

-- get :: (Unbox a) => RingBuffer a -> IO a
-- get (RB rb start end pos sz) = do
--   st <- R.readIORef start
--   e <- R.readIORef end
--   s <- R.readIORef sz
--   p <- R.readIORef pos
--   if st == e then error "Empty buffer"
--              else do
--                let nst = (st + 1) `mod` s
--                R.writeIORef start ((st + 1) `mod` s)
--                when (p == st) $ R.writeIORef pos nst
--                MV.unsafeRead rb st

-- readBuffer :: (Unbox a) => RingBuffer a -> IO (Int, a)
-- readBuffer (RB rb start end pos sz) = do
--   st <- R.readIORef start
--   e <- R.readIORef end
--   p <- R.readIORef pos
--   s <- R.readIORef sz
--   let np = (p + 1) `mod` s
--       np' = if np == e then st else np
--   R.writeIORef pos np'
--   (p, ) <$> MV.unsafeRead rb p

area :: Coords -> Coords -> Int
area (x, y) (x', y') =  (abs (x - x') + 1) * (abs (y - y') + 1)
{-# INLINE area #-}

getArea :: (Int, Rect) -> Int
getArea (a, _) = a
{-# INLINE getArea #-}

treatState :: FoldState -> IO ()
treatState (FS heap cs vs rb) = do
  csv <- R.readIORef cs
  coords <- (\case
                [y', x'] -> (x', y')
                _ -> undefined) <$> R.readIORef vs
  R.writeIORef vs []
  put rb coords
  let addHeap c' = push heap (area coords c', (coords, c'))
  mapM_ addHeap csv
  R.writeIORef cs (coords:csv)


folder :: FoldState -> () -> Word8 -> IO ()
folder fs@(FS _ _ vs _) _ w
  | w == _lf = treatState fs
  | w == _comma = R.modifyIORef' vs (0:)
  | isDigit w = R.modifyIORef' vs (\case
                                      [] -> [digit w]
                                      (a:as) -> (10 * a + digit w):as)
  | otherwise = undefined

cross :: Coords -> Coords -> Coords -> Int
cross (xa, ya) (xb, yb) (_, yc)
  | yb == yc || xa < xb || ya > max yb yc || ya <= min yb yc = 0
  |  otherwise = 1

onLine :: Coords -> Coords -> Coords -> Bool
onLine (xa, ya) (xb, yb) (xc, yc) = (xa - xb) * (yc - yb) == (ya - yb) * (xc - xb)
  && xa >= min xb xc && xa <= max xb xc && ya >= min yb yc && ya <= max yb yc

intersect :: Coords -> Coords -> Coords -> Coords -> Bool
intersect (xa, ya) (xb, yb) (xc, yc) (xd, yd)
  | xa == xb && yc == yd = min ya yb < yc && yc < max ya yb && min xc xd < xa && max xc xd > xa
  | ya == yb && xc == xd = min xa xb < xc && xc < max xa xb && min yc yd < ya && max yc yd > ya
  | otherwise = False

swapRect :: Coords -> Coords -> (Coords, Coords)
swapRect (xa, ya) (xb, yb) = ((xa, yb), (xb, ya))

searchLargestValid :: Heap (Int, Rect) -> RingBuffer Coords -> IO Int
searchLargestValid h (RB rb start end _ sz) = searchLargestValid'
  where
    searchLargestValid' = do
      (a, (ca, cb)) <- fromMaybe undefined <$> popMin h
      let (ca', cb') = swapRect ca cb
      st <- R.readIORef start
      e <- R.readIORef end
      s <- R.readIORef sz
      coords <- MV.unsafeRead rb st
      let isValid :: Coords -> Int -> Int -> Int -> Bool -> Bool -> IO Bool
          isValid c i v1 v2 ol1 ol2 = do
            let i' = (i + 1) `mod` s
            c' <- if i' == e then pure coords else MV.unsafeRead rb i'
            let v1' = v1 + cross ca' c c'
                v2' = v2 + cross cb' c c'
                ol1' = ol1 || onLine ca' c c'
                ol2' = ol2 || onLine cb' c c'
                int1 = intersect ca ca' c c'
                int2 = intersect ca cb' c c'
                int3 = intersect cb ca' c c'
                int4 = intersect cb cb' c c'
                hasInt = int1 || int2 || int3 || int4
            if i' == e then pure $ not hasInt && (ol1' || odd v1') && (ol2' || odd v2')
                       else (not hasInt &&) <$> isValid c' i' v1' v2' ol1' ol2'
      valid <- isValid coords st 0 0 False False
      if valid then pure a else searchLargestValid'

getLargest :: Stream IO Word8 -> IO Int
getLargest s = do
  h <- newHeap 122760 (comparing (Down . getArea))
  cs <- R.newIORef []
  vs <- R.newIORef []
  rb <- newBuffer 497
  let fs = FS h cs vs rb
  S.fold (F.foldlM' (folder fs) (pure ())) s
  getArea . fromMaybe undefined <$> popMin h

getLargestValid :: Stream IO Word8 -> IO Int
getLargestValid s = do
  h <- newHeap 122760 (comparing (Down . getArea))
  cs <- R.newIORef []
  vs <- R.newIORef []
  rb <- newBuffer 497
  let fs = FS h cs vs rb
  S.fold (F.foldlM' (folder fs) (pure ())) s
  searchLargestValid h rb

part1 :: Bool -> Stream IO Word8 -> IO ()
part1 _ s = getLargest s >>= print

part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ s = getLargestValid s >>= print
