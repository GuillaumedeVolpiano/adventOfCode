{-# LANGUAGE LambdaCase #-}
-- | Day8 module: solves a graph-based problem using incremental MST and disjoint sets.
-- Provides both part1 and part2 solutions, plus lower-level operations on disjoint sets
-- and a mutable priority queue.
module Day8
  ( part1
  , part2
  , connectAll
  , getCircuit
  , prodThree
  , findCircuits
  , findLastCircuits
  , DisjointSets (DS)
  ) where

import           Control.DeepSeq               (NFData, rnf)
import           Control.Monad                 (unless, void, when)
import           Data.Hashable                 (Hashable, hashWithSalt)
import           Data.IORef                    (IORef)
import qualified Data.IORef                    as R (modifyIORef', newIORef,
                                                     readIORef, writeIORef)
import qualified Data.List                     as L (sortBy)
import           Data.Maybe                    (fromMaybe)
import           Data.Ord                      (Down (Down), comparing)
import           Data.Vector.Primitive.Mutable (IOVector)
import qualified Data.Vector.Primitive.Mutable as MV (new, unsafeRead,
                                                      unsafeWrite)
import qualified Data.Vector.Unboxed.Mutable   as UMV (IOVector, unsafeNew,
                                                       unsafeRead, unsafeWrite)
import           Data.Word                     (Word8)
import           Data.Word8                    (_comma, _lf)
import           Helpers.General.Streamly      (digit, isDigit)
import qualified Streamly.Data.Fold            as F (foldlM')
import qualified Streamly.Data.Stream          as S (fold)
import           Streamly.Data.Stream          (Stream)

-- | 3D coordinates of a node.
data Coords = C {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Eq, Ord, Show)

-- | Disjoint sets with path compression and a reference to top three largest sets.
data DisjointSets = DS (IOVector Int) (IOVector Int) (IORef [(Int, Int)])

-- | Mutable priority queue for edges, storing current size in an IORef.
data PQueue = PQ !(UMV.IOVector Edge) !(IORef Int)

-- | Edge representation: (distance, node1, node2, x1, x2)
type Edge = (Int, Int, Int, Int, Int)

-- | Mutable fold state for streaming input parsing.
data FoldState = FS !PQueue !(IORef Int) !(IORef [(Coords, Int)]) !DisjointSets !(IORef [Int])

instance NFData Coords where
  rnf (C x y z) = rnf x `seq` rnf y `seq` rnf z

instance Hashable Coords where
  hashWithSalt s (C h w d) = hashWithSalt s (h, w, d)

instance NFData DisjointSets where
  rnf (DS p s tt) = rnf p `seq` rnf s `seq` rnf tt

-- | Initialize a singleton set for a node.
mkSet :: Int -> DisjointSets -> IO ()
mkSet c (DS p s _) = do
  MV.unsafeWrite p c c
  MV.unsafeWrite s c 1
{-# INLINE mkSet #-}

-- | Find the representative of a set with path compression.
find :: Int -> DisjointSets -> IO Int
find c ds@(DS p _ _) = do
  pc <- MV.unsafeRead p c
  if pc == c then pure c
             else do
               c' <- find pc ds
               MV.unsafeWrite p c c'
               pure c'

-- | Union two sets; update top three largest sets if needed.
union :: Int -> Int -> DisjointSets -> IO Int
union c c' ds@(DS p s tt) = do
  x <- find c ds
  y <- find c' ds
  xs <- MV.unsafeRead s x
  if x == y then pure xs
            else do
    ys <- MV.unsafeRead s y
    let (nr, nc) = if xs < ys then (y, x)
                              else (x, y)
    MV.unsafeWrite p nc nr >> MV.unsafeWrite s nr (xs + ys)
    topThree <- filter (\(co, _) -> (co /= x) && (co /= y)) <$> R.readIORef tt
    R.writeIORef tt (take 3 . L.sortBy (comparing (Down . snd)) $ (nr, xs + ys) : topThree)
    pure $ xs + ys

-- | Extract distance from an edge.
dist :: Edge -> Int
dist (x, _, _, _, _) = x

-- | Allocate a new priority queue with given capacity.
newQueue :: Int -> IO PQueue
newQueue sz = do
  pq <- UMV.unsafeNew sz
  s <- R.newIORef 0
  pure $ PQ pq s

-- | Push an edge into the priority queue, maintaining heap invariant.
push :: PQueue -> Edge -> IO ()
push p@(PQ pq sz) e = do
  i <- R.readIORef sz
  UMV.unsafeWrite pq i e
  R.writeIORef sz (i + 1)
  siftUp p i

-- | Bubble an element up to maintain min-heap property.
siftUp :: PQueue -> Int -> IO ()
siftUp p@(PQ pq _) i = when (i > 0) $ do
  let i' = div (i - 1) 2
  v <- UMV.unsafeRead pq i
  v' <- UMV.unsafeRead pq i'
  when (dist v < dist v') $ do
    UMV.unsafeWrite pq i' v
    UMV.unsafeWrite pq i v'
    siftUp p i'

-- | Pop the minimum edge from the priority queue, if non-empty.
popMin :: PQueue -> IO (Maybe Edge)
popMin p@(PQ pq sz) = do
  i <- R.readIORef sz
  if i == 0 then pure Nothing
            else do
              v <- UMV.unsafeRead pq 0
              v' <- UMV.unsafeRead pq (i - 1)
              UMV.unsafeWrite pq 0 v'
              siftDown p 0
              pure $ Just v

-- | Push down an element to maintain min-heap property.
siftDown :: PQueue -> Int -> IO ()
siftDown p@(PQ pq sz) i = do
  s <- R.readIORef sz
  let left = 2 * i + 1
      right = 2 * i + 2
  unless (left >= s) $ do
    v <- UMV.unsafeRead pq i
    v' <- UMV.unsafeRead pq left
    (ni, nv) <- if right < s then do
                                  v''<- UMV.unsafeRead pq right
                                  if dist v' <= dist v'' then pure (left, v') else pure (right, v'')
                                  else pure (left, v')
    when (dist v > dist nv) $ do
      UMV.unsafeWrite pq i nv
      UMV.unsafeWrite pq ni v
      siftDown p ni

-- | Squared Euclidean distance between two coordinates.
distance :: Coords -> Coords -> Int
distance (C h w d) (C h' w' d') = ((h - h')^(2 :: Int)) + ((w - w')^(2 :: Int)) + ((d - d')^(2 :: Int))
{-# INLINE distance #-}

-- | Process accumulated coordinates on line feed; push new edges to PQ and update sets.
treatState :: FoldState -> IO ()
treatState (FS pq idx cs ds xs) = do
  csv <- R.readIORef cs
  coords@(C x _ _)<- (\case
                        [z, y, x] -> C x y z
                        _ -> undefined) <$> R.readIORef xs
  R.writeIORef xs []
  p <- R.readIORef idx
  let addQueue (coords'@(C x' _ _), p') = push pq (distance coords coords', p, p', x, x')
  mapM_ addQueue csv
  mkSet p ds
  R.writeIORef cs ((coords, p) : csv)
  R.writeIORef idx (p + 1)

-- | Fold function for parsing input stream incrementally.
mutableFold :: FoldState -> () -> Word8 -> IO ()
mutableFold fs@(FS _ _ _ _ xs) _ w
  | w == _lf = treatState fs
  | w == _comma = R.modifyIORef' xs (0:)
  | isDigit w = R.modifyIORef' xs (\case
                                      [] -> [digit w]
                                      (a:as) -> (10 * a + digit w):as)
  | otherwise = undefined

-- | Extract a fixed number of edges from the PQ and merge corresponding sets.
getCircuit :: Int -> PQueue -> DisjointSets -> IO ()
getCircuit c pq circuit = subCircuit c
  where
    subCircuit 0 = pure ()
    subCircuit c' = do
      mPairQueue <- popMin pq
      let (_, coords, coords', _, _) = fromMaybe undefined mPairQueue
      void $ union coords coords' circuit
      subCircuit (c' - 1)

-- | Return the product of sizes of the three largest sets.
prodThree :: DisjointSets -> IO Int
prodThree (DS _ _ topThree) = product . map snd <$> R.readIORef topThree

-- | Incrementally union edges until all nodes are connected; return product of last edge's x-values.
connectAll :: Int -> PQueue -> DisjointSets -> IO Int
connectAll sz pq ds = crawl
  where
    crawl = do
      mPairQueue <- popMin pq
      let (_, coords, coords', x, x') = fromMaybe undefined mPairQueue
      ns <- union coords coords' ds
      if ns == sz then pure (x * x') else crawl

-- | Parse input stream and fully connect graph; return final MST value for part 2.
findLastCircuits :: Stream IO Word8 -> IO Int
findLastCircuits s = do
  p <- MV.new 1000
  si <- MV.new 1000
  tt <- R.newIORef []
  let circuits = DS p si tt
  idx <- R.newIORef 0
  cs <- R.newIORef []
  v <- R.newIORef []
  pq <- newQueue 499500
  let fs = FS pq idx cs circuits v
  S.fold (F.foldlM' (mutableFold fs) (pure ())) s
  sz <- R.readIORef idx
  connectAll sz pq circuits

-- | Parse input stream and compute MST for part 1; optionally use test size.
findCircuits :: Bool -> Stream IO Word8 -> IO Int
findCircuits isTest s = do
    p <- MV.new 1000
    si <- MV.new 1000
    tt <- R.newIORef []
    let circuits = DS p si tt
    idx <- R.newIORef 0
    cs <- R.newIORef []
    v <- R.newIORef []
    pq <- newQueue (if isTest then 380 else 499500)
    let fs = FS pq idx cs circuits v
    S.fold (F.foldlM' (mutableFold fs) (pure ())) s
    let ind = if isTest then 10 else 1000
    getCircuit ind pq circuits
    prodThree circuits

-- | Solve part 1: compute product of sizes of the three largest connected components.
part1 :: Bool -> Stream IO Word8 -> IO ()
part1 isTest s = findCircuits isTest s >>= print

-- | Solve part 2: compute product of last connected edge's coordinates in full graph.
part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ s = findLastCircuits s >>= print
