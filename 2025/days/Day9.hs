{-# LANGUAGE LambdaCase #-}
module Day9
  ( part1
  , part2
  , Rect
  , getLargest
  , getLargestValid
  ) where

import           Control.Monad                 (forM_, (>=>))
import qualified Data.Set                      as St (fromList, toList)
import           Data.Vector.Primitive         (Vector)
import qualified Data.Vector.Primitive         as V (fromList, length,
                                                     unsafeFreeze, (!))
import           Data.Vector.Primitive.Mutable (IOVector)
import qualified Data.Vector.Primitive.Mutable as MV (replicate, unsafeRead,
                                                      unsafeWrite)
import           Data.Word                     (Word8)
import           Data.Word8                    (_comma, _lf)
import           Helpers.General.Streamly      (digit, isDigit)
import qualified Streamly.Data.Fold            as F (foldl')
import qualified Streamly.Data.Stream          as S (fold)
import           Streamly.Data.Stream          (Stream)

type Rect = (Coords, Coords)

type Coords = (Int, Int)

data FoldState = ReadX !Int ![Coords]
               | ReadY !Int !Int ![Coords]

area :: Coords -> Coords -> Int
area (x, y) (x', y') =  (abs (x - x') + 1) * (abs (y - y') + 1)
{-# INLINE area #-}

folder :: FoldState -> Word8 -> FoldState
folder (ReadX x coords) w
  | w == _comma = ReadY x 0 coords
  | isDigit w = ReadX (10 * x + digit w) coords
  | otherwise = undefined
folder (ReadY x y coords) w
  | w == _lf = ReadX 0 $ (x,y) : coords
  | isDigit w = ReadY x (10*y + digit w) coords
  | otherwise = undefined

getCoords :: Stream IO Word8 -> IO [Coords]
getCoords s = S.fold (F.foldl' folder (ReadX 0 [])) s
                >>= \case
                  ReadY {} -> error "partial read"
                  ReadX _ cs -> pure . reverse $ cs

getLargest :: Stream IO Word8 -> IO Int
getLargest s = do
  let getAreas []       = 0
      getAreas [_]      = 0
      getAreas (a : as) = max (maximum . map (area a) $ as) $ getAreas as
  getAreas <$> getCoords s

getLargestValid :: Stream IO Word8 -> IO Int
getLargestValid s = do
  coords <- getCoords s
  let (xs, ys) = extractXY coords
      l = 2 * V.length xs + 1
      h = 2 * V.length ys + 1
  grid <- buildGrid coords h l xs ys
  fillGrid h l grid
  sums <- buildSums h l xs ys grid
  let coords' = map (\(x, y) -> ((x, y), (getBand x xs, getBand y ys))) coords
      checkRectangles [] = 0
      checkRectangles [_] = 0
      checkRectangles (c:cs) = max (maximum . map (checkRectangle sums l c) $ cs) $ checkRectangles cs
  pure . checkRectangles $ coords'

extractXY :: [Coords] -> (Vector Int, Vector Int)
extractXY coords = (mkVec . map fst $ coords, mkVec . map snd $ coords)
  where
    mkVec = V.fromList . St.toList . St.fromList

getBand :: Int -> Vector Int -> Int
getBand v vec = dc 0 (V.length vec - 1)
  where
    dc lo hi
      | vec V.! i == v = 2 * i
      | vec V.! i < v = dc (i + 1) hi
      | otherwise = dc lo (i - 1)
      where
        i = (hi + lo) `div` 2

getBandWidth :: Int -> Vector Int -> Int
getBandWidth b vec
  | even b = 1
  | otherwise = (vec V.! (i + 1)) - (vec V.! i) - 1
  where
    i = b `div` 2

buildGrid :: [Coords] -> Int -> Int -> Vector Int -> Vector Int -> IO (IOVector Word8)
buildGrid coords@(c:oords) h l xs ys = do
  let pairs = zip coords (oords ++ [c])
  grid <- MV.replicate (l * h) 0
  forM_ pairs $ \((xa, ya), (xb, yb)) -> do
    let ia = getBand xa xs
        ib = getBand xb xs
        ja = getBand ya ys
        jb = getBand yb ys
    if xa == xb then forM_ [min ja jb..max ja jb] $ \j ->
                  MV.unsafeWrite grid (cell ia j l) 1
                else forM_ [min ia ib .. max ia ib] $ \i ->
                  MV.unsafeWrite grid (cell i ja l) 1
  pure grid
buildGrid [] _ _ _ _ = error "empty coords list"

fillGrid :: Int -> Int -> IOVector Word8 -> IO ()
fillGrid h l grid = do
  let fifo = [(0, 0)]
      filler [] = pure ()
      filler ((x, y):ps) = do
        if x < 0 || y < 0 || x >= l || y >= h
          then filler ps
          else do
            v <- MV.unsafeRead grid (y * l + x)
            if v == 0 then do
                        MV.unsafeWrite grid (y * l + x) 2
                        filler ((x + 1, y):(x - 1, y):(x, y + 1): (x, y - 1) : ps)
                      else filler ps
  filler fifo

cell :: Int -> Int -> Int -> Int
cell x y l = (y + 1) * l + x+ 1

printGrid :: Int ->  Int -> IOVector Word8 -> IO ()
printGrid h l grid = do
  forM_ [0 .. h - 1] $ \y -> do
    forM_ [y * l .. (y + 1) * l - 1] $
      MV.unsafeRead grid >=> \case
        0 -> putStr "."
        1 -> putStr "#"
        2 -> putStr "o"
        _ -> error "undefined value"
    putStr "\n"

buildSums :: Int -> Int -> Vector Int -> Vector Int -> IOVector Word8 -> IO (Vector Int)
buildSums h l xs ys grid = do
  sums <- MV.replicate ( l * h ) 0
  forM_ [1 .. l - 1] $ \x -> do
    forM_ [1 .. h - 1] $ \y -> do
      v <- MV.unsafeRead grid (y * l + x)
      let w = if v == 2
                 then 0
                  else getBandWidth (x - 1) xs * getBandWidth (y - 1) ys
      s1 <- MV.unsafeRead sums (y * l + x - 1)
      s2 <- MV.unsafeRead sums ((y - 1) * l + x)
      s3 <- MV.unsafeRead sums ((y - 1) * l + x - 1)
      MV.unsafeWrite sums (y * l + x) (w + s1 + s2 - s3)
  V.unsafeFreeze sums

checkRectangle :: Vector Int -> Int -> (Coords, Coords) -> (Coords, Coords) -> Int
checkRectangle sums l (ca, (ia, ja)) (cb, (ib, jb))
  | area ca cb == tiles = tiles
  | otherwise = 0
  where
    i1 = min ia ib
    i2 = max ia ib
    j1 = min ja jb
    j2 = max ja jb
    s1 = sums V.! cell i2 j2 l
    s2 = sums V.! cell (i1 - 1) j2 l
    s3 = sums V.! cell i2 (j1 - 1) l
    s4 = sums V.! cell (i1 - 1) (j1 - 1) l
    tiles = s1 - s2 - s3 + s4

part1 :: Bool -> Stream IO Word8 -> IO ()
part1 _ s = getLargest s >>= print

part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ s = getLargestValid s >>= print
