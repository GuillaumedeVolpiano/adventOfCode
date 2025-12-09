{-# LANGUAGE LambdaCase #-}
module Day9
  ( part1
  , part2
  , getLargest
  ) where

import           Streamly.Data.Stream (Stream)
import           Data.Word (Word8)
import Helpers.Heap.Primitive.Mutable (Heap, newHeap, push, popMin)
import Data.Ord (comparing, Down(Down))
import Data.IORef (IORef)
import Data.Word8 (_lf, _comma)
import Helpers.General.Streamly (digit, isDigit)
import qualified Data.IORef as R (modifyIORef', readIORef, writeIORef, newIORef)
import qualified Streamly.Data.Stream as S (fold)
import qualified Streamly.Data.Fold as F (foldlM')
import Data.Maybe (fromMaybe)

type Areas = Heap Int

data Coords = C {-# UNPACK #-} !Int {-# UNPACK #-} !Int

data FoldState = FS !Areas !(IORef [Coords]) !(IORef [Int])

area :: Coords -> Coords -> Int
area (C x y) (C x' y') =  (abs (x - x') + 1) * (abs (y - y') + 1)

treatState :: FoldState -> IO ()
treatState (FS heap cs vs) = do
  csv <- R.readIORef cs
  coords <- (\case
              [y, x] -> C x y
              _ -> undefined) <$> R.readIORef vs
  R.writeIORef vs []
  let addHeap c' = push heap (area coords c')
  mapM_ addHeap csv
  R.writeIORef cs (coords:csv)


folder :: FoldState -> () -> Word8 -> IO ()
folder fs@(FS _ _ vs) _ w
  | w == _lf = treatState fs
  | w == _comma = R.modifyIORef' vs (0:)
  | isDigit w = R.modifyIORef' vs (\case
                                      [] -> [digit w]
                                      (a:as) -> (10 * a + digit w):as)
  | otherwise = undefined

getLargest :: Stream IO Word8 -> IO Int
getLargest s = do
  h <- newHeap 499500 (comparing Down)
  cs <- R.newIORef []
  vs <- R.newIORef []
  let fs = FS h cs vs
  S.fold (F.foldlM' (folder fs) (pure ())) s
  fromMaybe undefined <$> popMin h

part1 :: Bool -> Stream IO Word8 -> IO ()
part1 _ s = getLargest s >>= print

part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ _ = print "Part 2"
