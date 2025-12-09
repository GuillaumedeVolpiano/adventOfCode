module Helpers.Heap.Primitive.Mutable (
                                      Heap
                                    , newHeap
                                    , push
                                    , popMin
                                    )
where
import           Control.Monad                 (unless, when)
import           Data.Vector.Primitive.Mutable (IOVector, Prim, unsafeNew,
                                                unsafeRead, unsafeWrite)
import           GHC.IORef                     (IORef, newIORef, readIORef,
                                                writeIORef)

data Heap a = H (a -> a -> Ordering) !(IOVector a) !(IORef Int)

-- | Allocate a new heap with given capacity and ordering.
newHeap :: (Prim a) => Int -> (a -> a -> Ordering) -> IO (Heap a)
newHeap sz ord = do
  pq <- unsafeNew sz
  s <- newIORef 0
  pure $ H ord pq s

-- | Push an edge into the priority queue, maintaining heap invariant.
push :: (Prim a) => Heap a -> a -> IO ()
push p@(H _ pq sz) e = do
  i <- readIORef sz
  unsafeWrite pq i e
  writeIORef sz (i + 1)
  siftUp p i

-- | Bubble an element up to maintain min-heap property.
siftUp :: (Prim a) => Heap a -> Int -> IO ()
siftUp p@(H ord pq _) i = when (i > 0) $ do
  let i' = div (i - 1) 2
  v <- unsafeRead pq i
  v' <- unsafeRead pq i'
  when (ord v v' == LT) $ do
    unsafeWrite pq i' v
    unsafeWrite pq i v'
    siftUp p i'

-- | Pop the minimum edge from the priority queue, if non-empty.
popMin :: (Prim a) => Heap a -> IO (Maybe a)
popMin p@(H _ pq sz) = do
  i <- readIORef sz
  if i == 0 then pure Nothing
            else do
              v <- unsafeRead pq 0
              v' <- unsafeRead pq (i - 1)
              unsafeWrite pq 0 v'
              siftDown p 0
              pure $ Just v

-- | Push down an element to maintain min-heap property.
siftDown :: (Prim a) => Heap a -> Int -> IO ()
siftDown p@(H ord pq sz) i = do
  s <- readIORef sz
  let left = 2 * i + 1
      right = 2 * i + 2
  unless (left >= s) $ do
    v <- unsafeRead pq i
    v' <- unsafeRead pq left
    (ni, nv) <- if right < s then do
                                  v''<- unsafeRead pq right
                                  if ord v' v'' /= GT  then pure (left, v') else pure (right, v'')
                                  else pure (left, v')
    when (ord v nv == GT) $ do
      unsafeWrite pq i nv
      unsafeWrite pq ni v
      siftDown p ni


