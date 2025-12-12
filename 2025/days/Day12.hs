{-# LANGUAGE TupleSections #-}
module Day12
  ( part1
  , part2
  , validGrids
  ) where

import           Control.Monad            (void, filterM)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Bits                (shiftL, shiftR, (.&.), (.|.))
import qualified Data.IntSet              as IS (fromList, union, toList)
import           Data.Vector              (Vector)
import qualified Data.Vector              as V (fromList, (!), length)
import           Data.Vector.Mutable      (IOVector)
import qualified Data.Vector.Mutable      as MV (replicate,
                                                 write, read)
import           Data.Word                (Word8)
import           Data.Word8               (_colon, _lf, _period, _space, _x)
import           Helpers.General.Streamly (digit, isDigit)
import qualified Streamly.Data.Fold       as F (drain, foldl', foldlM', toList)
import           Streamly.Data.Fold       (Fold)
import qualified Streamly.Data.Parser     as P (eof, many, manyTill, one,
                                                satisfy, takeEQ)
import           Streamly.Data.Parser     (Parser)
import           Streamly.Data.Stream     (Stream)
import           Data.IORef (IORef, newIORef, modifyIORef', readIORef, writeIORef)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM (insert, lookup)
import qualified Streamly.Data.Stream as S (parse)
import Debug.Trace

data Grid = G (IORef Int) (IOVector Int) Int Int
              (IORef (HashMap (Int,Int, Int, Int) Bool))

type Shapes = Vector [Int]

parseGrid :: Parser Word8 IO Grid
parseGrid = do
  w <- P.manyTill (P.satisfy isDigit) (P.satisfy (==_x)) toNumber
  h <- P.manyTill (P.satisfy isDigit) (P.satisfy (==_colon)) toNumber
  s <- liftIO $ newIORef 0
  P.manyTill (P.satisfy (==_space) >> P.many (P.satisfy isDigit) toNumber) (P.satisfy (==_lf)) (toStock s)
  tot <- liftIO $ readIORef s
  let remBits = (h * w) `shiftL` 42
  liftIO $ writeIORef s (remBits + tot)
  g <- liftIO $ MV.replicate h 0
  m <- liftIO $ newIORef mempty
  pure $ G s g w h m

toNumber :: Fold IO Word8 Int
toNumber = flip F.foldl' 0 $ \acc w -> acc * 10 + digit w

toStock :: IORef Int -> Fold IO Int ()
toStock s = flip F.foldlM' (pure ()) $ \_ v -> modifyIORef' s ((+ v) . flip shiftL 7)

parseGrids :: Parser Word8 IO [Grid]
parseGrids = P.manyTill parseGrid P.eof F.toList

parseShape :: Parser Word8 IO [Int]
parseShape = do
  P.manyTill P.one (P.satisfy (==_lf)) F.drain
  l1 <- P.takeEQ 3 toBits
  void $ P.satisfy (==_lf)
  l2 <- P.takeEQ 3 toBits
  void $ P.satisfy (==_lf)
  l3 <- P.takeEQ 3 toBits
  let n = (l1 `shiftL` 6) + (l2 `shiftL` 3) + l3
  void $ P.satisfy (==_lf)
  void $ P.satisfy (==_lf)
  pure $ flipRotate n

toBits :: Fold IO Word8 Int
toBits = flip F.foldl' 0 $ \acc w -> (acc `shiftL` 1) +
  if w == _period then 0 else 1

flipRotate :: Int -> [Int]
flipRotate i = IS.toList $ IS.fromList (rotations i) `IS.union` IS.fromList
  (rotations . mirror $ i)
  where
    rotations = take 4 . iterate rotate

rotate :: Int -> Int
rotate i = moveBit 0 2 .|. moveBit 1 5 .|. moveBit 2 8 .|. moveBit 3 1
  .|. moveBit 5 7 .|. moveBit 6 0 .|. moveBit 7 3 .|. moveBit 8 6
  .|. (i .&. 16)
  where
    moveBit f t = ((i `shiftR` f) .&. 1) `shiftL` t

mirror :: Int -> Int
mirror i = moveBit 0 2 .|. moveBit 2 0 .|. moveBit 3 5 .|. moveBit 5 3
  .|. moveBit 6 8 .|. moveBit 8 6 .|. (i .&. 146)
  where
    moveBit f t = ((i `shiftR` f) .&. 1) `shiftL` t

parseShapes :: Parser Word8 IO Shapes
parseShapes = V.fromList <$> P.many parseShape F.toList

parseInput :: Parser Word8 IO (Shapes, [Grid])
parseInput = parseShapes >>= \s -> (s,) <$> parseGrids

insertShapeAt :: Int -> Int -> Int -> Int -> Int -> Grid -> IO Bool
insertShapeAt i n1 n2 n3 s (G _ g w _ _) = do
      let p = i `mod` w
          r = i `div` w
          l1 = (s `shiftR` 6) `shiftL` p
          l2 = (s `shiftR` 3 .&. 7) `shiftL` p
          l3 = (s .&. 7) `shiftL` p
      if (l1 .&. n1) .|. (l2 .&. n2) .|. (l3 .&. n3) == 0
          then do
            MV.write g r (n1 .|. l1)
            MV.write g (r + 1) (n2 .|. l2)
            MV.write g (r + 2) (n3 .|. l3)
            pure True
          else
            pure False

dfs :: Int -> Shapes -> Grid -> IO Bool
dfs i shapes g@(G s gr w h m)
  | i `mod` w > w - 3 = dfs (w * ((i `div` w) + 1)) shapes g
  | otherwise = do
      st <- readIORef s
      mem <- readIORef m
      let sto = st .&. ((1 `shiftL` 42) -1)
          next
            | sto == 0 = pure True
            | i `div` w > h - 3 = pure False
            | otherwise = do
                let r = i `div` w
                n1 <- MV.read gr r
                n2 <- MV.read gr (r + 1)
                n3 <- MV.read gr (r + 2)
                let continue
                      | fullGrid st = writeIORef m (HM.insert (st, n1, n2, n3) False mem)
                         >> pure False
                      | otherwise = do
                          cur <- or <$> mapM (processShapeSet i shapes g st n1 n2 n3) [0..5]
                          writeIORef m (HM.insert (st, n1, n2, n3) cur mem)
                          (cur ||) <$> dfs (i + 1) shapes g
                case HM.lookup (st, n1, n2, n3) mem of
                  Just v -> pure v
                  _ -> continue
      next

processShapeSet :: Int -> Shapes -> Grid -> Int -> Int -> Int -> Int -> Int -> IO Bool
processShapeSet i shapes g@(G s _ _ _ _) st n1 n2 n3 p
  | (st `shiftR` (7 * (5 - p))) .&. 127 == 0 = pure False
  | otherwise = do
      let sps = shapes V.! p
          nv = st - (1 `shiftL` (7 * (5 - p))) - (7 `shiftL` 42)
      writeIORef s nv
      cur <- or <$> mapM (processShape i shapes g n1 n2 n3) sps
      writeIORef s st
      pure cur

processShape :: Int -> Shapes -> Grid -> Int -> Int -> Int -> Int -> IO Bool
processShape i shapes g@(G _ gr w _ _) n1 n2 n3 shape = do
  res <- insertShapeAt i shape n1 n2 n3 g
  if res then do
            let r = i `div` w
            cur <- dfs (i + 1) shapes g
            MV.write gr r n1
            MV.write gr (r + 1) n2
            MV.write gr (r + 2) n3
            pure cur
          else pure False

fullGrid :: Int -> Bool
fullGrid st = space < 7 * (zero + one + two + three + four + five)
  where
    space = st `shiftR` 42
    zero = st `shiftR` 35 .&. 127
    one = st `shiftR` 28 .&. 127
    two = st `shiftR` 21 .&. 127
    three = st `shiftR` 14 .&. 127
    four = st `shiftR` 7 .&. 127
    five = st .&. 127

validGrids :: Stream IO Word8 -> IO Int
validGrids s = do
  (shapes, grids) <- either (error "Parser error") id <$> S.parse parseInput s
  traceIO ("Shape counts " ++ show (V.length shapes))
  traceIO ("grids counts: " ++ show (length grids))
  length <$> filterM (dfs 0 shapes) grids

part1 :: Bool -> Stream IO Word8 -> IO ()
part1 _ s = validGrids s >>= print

part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ _ = print "Part 2"
