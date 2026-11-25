
-- | Day8 module: solves a graph-based problem using incremental MST and
-- disjoint sets.
-- Provides both part1 and part2 solutions, plus lower-level operations on disjoint sets
-- and a mutable priority queue.
module Day8
  ( part1
  , part2
  , findCircuits
  , findLastCircuits
  , DisjointSets (DS)
  ) where

import           Control.DeepSeq               (NFData, rnf)
import           Control.Monad                 (forM_, when)
import           Data.Bits                     (shiftL, shiftR, (.&.), (.|.))
import           Data.Foldable                 (foldrM)
import qualified Data.List                     as L (sortBy)
import           Data.Maybe                    (catMaybes)
import           Data.Ord                      (Down (Down), comparing)
import           Data.Vector.Algorithms.Intro  (select)
import           Data.Vector.Primitive.Mutable (IOVector)
import qualified Data.Vector.Primitive.Mutable as MV (generate, length, mapM_,
                                                      replicate, unsafeNew,
                                                      unsafeRead, unsafeTake,
                                                      unsafeWrite)
import           Data.Word                     (Word8)
import           Data.Word8                    (_comma, _lf)
import           Helpers.General.Streamly      (digit, isDigit)
import qualified Streamly.Data.Fold            as F (foldlM')
import qualified Streamly.Data.Stream          as S (fold)
import           Streamly.Data.Stream          (Stream)

-- | Disjoint sets with path compression and a reference to top three largest sets.
data DisjointSets = DS (IOVector Int) (IOVector Int)

-- | Mutable fold state for streaming input parsing.
data FoldState
  = ReadX !Int !Int
  | ReadY !Int !Int
  | ReadZ !Int !Int

instance NFData DisjointSets where
  rnf (DS p s) = rnf p `seq` rnf s


-- | Find the representative of a set with path compression.
find :: Int -> DisjointSets -> IO Int
find c ds@(DS p _) = do
  pc <- MV.unsafeRead p c
  if pc == c then pure c
             else do
               c' <- find pc ds
               MV.unsafeWrite p c c'
               pure c'

-- | Union two sets; update top three largest sets if needed.
union :: Int -> Int -> DisjointSets -> IO Int
union c c' ds@(DS p s) = do
  x <- find c ds
  y <- find c' ds
  xs <- MV.unsafeRead s x
  if x == y then pure xs
            else do
    ys <- MV.unsafeRead s y
    let (nr, nc) = if xs < ys then (y, x)
                              else (x, y)
    MV.unsafeWrite p nc nr >> MV.unsafeWrite s nr (xs + ys)
    pure $ xs + ys


-- | Parse input stream and fully connect graph; return final MST value for part 2.
findLastCircuits :: Stream IO Word8 -> IO Int
findLastCircuits s = do
  (xs, ys, zs) <- getBoxes s
  (inTree, best, from) <- initTree xs ys zs
  (_, f, j) <- prim xs ys zs inTree best from
  xf <- MV.unsafeRead xs f
  xj <- MV.unsafeRead xs j
  pure $ xf * xj

-- | Parse input stream and compute MST for part 1; optionally use test size.
findCircuits :: Bool -> Stream IO Word8 -> IO Int
findCircuits isBool s = do
  (xs, ys, zs) <- getBoxes s
  edges <- makeEdges xs ys zs
  shortests <- getShortest isBool edges
  let n = MV.length xs
  parents <- MV.generate n id
  sizes <- MV.replicate n (1 :: Int)
  let ds = DS parents sizes
      unionize key = union ((key `shiftR` 10) .&. 1023) (key .&. 1023) ds
  MV.mapM_ unionize shortests
  let maybeSize i = MV.unsafeRead parents i >>=
                      \p -> if i == p
                                then Just <$> MV.unsafeRead sizes i
                                else pure Nothing
  product . take 3 . L.sortBy (comparing Down) . catMaybes <$> mapM maybeSize [0..n-1]

folder :: IOVector Int -> IOVector Int -> IOVector Int -> FoldState -> Word8 -> IO FoldState
folder xs _ _(ReadX x i) w
  | w == _comma = MV.unsafeWrite xs i x >> pure (ReadY 0 i)
  | isDigit w = pure $ ReadX (10 * x + digit w) i
  | otherwise = undefined
folder _ ys _ (ReadY y i) w
  | w == _comma = MV.unsafeWrite ys i y >> pure (ReadZ 0 i)
  | isDigit w = pure $ ReadY (10 * y + digit w) i
  | otherwise = undefined
folder _ _ zs (ReadZ z i) w
  | w == _lf = MV.unsafeWrite zs i z >> pure (ReadX 0  (i + 1))
  | isDigit w = pure $ ReadZ (10 * z + digit w) i
  | otherwise = undefined

getBoxes :: Stream IO Word8 -> IO (IOVector Int, IOVector Int, IOVector Int)
getBoxes s = do
  xsm <- MV.unsafeNew 1000
  ysm <- MV.unsafeNew 1000
  zsm <- MV.unsafeNew 1000
  fs <- S.fold (F.foldlM' (folder xsm ysm zsm) (pure $ ReadX 0 0)) s
  case fs of
    ReadX 0 i -> do
      let xs = MV.unsafeTake i xsm
          ys = MV.unsafeTake i ysm
          zs = MV.unsafeTake i zsm
      pure (xs, ys, zs)
    _ -> error "failed to properly get the boxes"

makeEdges :: IOVector Int -> IOVector Int -> IOVector Int -> IO (IOVector Int)
makeEdges xs ys zs = do
  let n = MV.length xs
  edges <- MV.unsafeNew (n * (n - 1) `div` 2)
  forM_ [0.. n - 2] $ \i -> do
      xi <- MV.unsafeRead xs i
      yi <- MV.unsafeRead ys i
      zi <- MV.unsafeRead zs i
      forM_ [i + 1 .. n - 1] $ \j -> do
        xj <- MV.unsafeRead xs j
        yj <- MV.unsafeRead ys j
        zj <- MV.unsafeRead zs j
        let d = (xi - xj)^(2 :: Int) + (yi - yj)^(2 :: Int) + (zi -zj)^(2 :: Int)
            key = (d `shiftL` 20) .|. (i `shiftL` 10) .|. j
            p = i * (2 * n - i -1) `div` 2 + j - i - 1
        MV.unsafeWrite edges p key
  pure edges

getShortest :: Bool -> IOVector Int -> IO (IOVector Int)
getShortest isBool edges = do
    let k = if isBool then 10 else 1000
    select edges k
    pure $ MV.unsafeTake k edges

initTree :: IOVector Int -> IOVector Int -> IOVector Int -> IO (IOVector Word8, IOVector Int, IOVector Int)
initTree xs ys zs = do
  let n = MV.length xs
  inTree <- MV.replicate n 0
  MV.unsafeWrite inTree 0 1
  best <- MV.unsafeNew n
  x0 <- MV.unsafeRead xs 0
  y0 <- MV.unsafeRead ys 0
  z0 <- MV.unsafeRead zs 0
  MV.unsafeWrite best 0 0
  forM_ [1 .. n-1] $ \i -> do
    xi <- MV.unsafeRead xs i
    yi <- MV.unsafeRead ys i
    zi <- MV.unsafeRead zs i
    MV.unsafeWrite best i ((xi - x0)^(2 :: Int) + (yi - y0)^(2 :: Int) + (zi -z0)^(2 :: Int))
  from <- MV.replicate n 0
  pure (inTree, best, from)

findClosest :: IOVector Word8 -> IOVector Int -> IO Int
findClosest inTree best = do
  let n = MV.length best
      isBest j (i, dist) = do
        isIn <- MV.unsafeRead inTree j
        if isIn == 0
           then do
             distj <- MV.unsafeRead best j
             if distj < dist then pure (j, distj)
                             else pure (i, dist)
           else pure (i, dist)
  (closest, _) <- foldrM isBest (undefined, maxBound) [0 .. n - 1]
  pure closest

primRound :: IOVector Int -> IOVector Int -> IOVector Int -> IOVector Word8 -> IOVector Int -> IOVector Int -> IO (Int, Int, Int)
primRound xs ys zs inTree best from = do
  let n = MV.length xs
  j <- findClosest inTree best
  d <- MV.unsafeRead best j
  f <- MV.unsafeRead from j
  MV.unsafeWrite inTree j 1
  xj <- MV.unsafeRead xs j
  yj <- MV.unsafeRead ys j
  zj <- MV.unsafeRead zs j
  forM_ [0 .. n-1] $ \t -> do
    it <- MV.unsafeRead inTree t
    when (it == 0) $ do
         xt <- MV.unsafeRead xs t
         yt <- MV.unsafeRead ys t
         zt <- MV.unsafeRead zs t
         bt <- MV.unsafeRead best t
         let djt = (xj - xt)^(2 :: Int) + (yj - yt)^(2 :: Int) + (zj - zt)^(2 :: Int)
         when (djt < bt) $ do
              MV.unsafeWrite best t djt
              MV.unsafeWrite from t j
  pure (d, f, j)

prim :: IOVector Int -> IOVector Int -> IOVector Int -> IOVector Word8 -> IOVector Int -> IOVector Int -> IO (Int, Int, Int)
prim xs ys zs inTree best from = foldrM doRound (0, 0, 0) [1 .. n - 1]
  where
    n = MV.length xs
    doRound _ (d, f, j) = do
      (d', f', j') <- primRound xs ys zs inTree best from
      if d < d' then pure (d', f', j')
                else pure (d, f, j)

-- | Solve part 1: compute product of sizes of the three largest connected components.
part1 :: Bool -> Stream IO Word8 -> IO ()
part1 isTest s = findCircuits isTest s >>= print

-- | Solve part 2: compute product of last connected edge's coordinates in full graph.
part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ s = findLastCircuits s >>= print
