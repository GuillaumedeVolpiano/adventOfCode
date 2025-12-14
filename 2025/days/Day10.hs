module Day10
  ( part1
  , part2
  , parseMachines
  ) where

import           Control.Monad            (void, foldM_, when)
import           Data.Bifunctor           (bimap)
import           Data.Bits                (setBit, shiftL, xor)
import           Data.Word                (Word8)
import           Data.Word8               (_braceleft, _braceright,
                                           _bracketleft, _bracketright, _comma,
                                           _lf, _numbersign, _parenleft,
                                           _parenright, _space)
import           Helpers.General.Streamly (digit, isDigit)
import qualified Streamly.Data.Fold       as F (foldl', foldr', foldlM', toList)
import           Streamly.Data.Fold       (Fold)
import qualified Streamly.Data.Parser     as P (deintercalate, eof, many,
                                                manyTill, one, satisfy)
import           Streamly.Data.Parser     (Parser)
import qualified Streamly.Data.Stream     as S (parse)
import           Streamly.Data.Stream     (Stream)
import Control.Monad.IO.Class (liftIO)
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as MV (replicate, replicateM, unsafeWrite, unsafeRead, unsafeNew, unsafeSwap, unsafeModify, length)
import qualified Data.Vector as V (freeze)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Debug.Trace ( traceIO )
import Data.Foldable (foldrM)

data Machine = M {-# UNPACK #-} !Int ![Int] Matrix

data Matrix = Mx {-# UNPACK #-} !Int {-# UNPACK #-} !Int (IOVector (IOVector Int))

data RingBuffer a = RB !(IORef Int) !(IORef Int) !(IORef Int) {-# UNPACK #-} !Int !(IOVector a)

newRingBuffer :: Int -> IO (RingBuffer a)
newRingBuffer cap = do
  rb <- MV.unsafeNew cap
  pos <- newIORef 0
  end <- newIORef 0
  size <- newIORef 0
  pure $ RB pos end size cap rb

pop :: RingBuffer a -> IO a
pop (RB pos _ size cap rb) = do
  s <- readIORef size
  if s == 0
     then error "RingBuffer is empty"
     else do
        p <- readIORef pos
        v <- MV.unsafeRead rb p
        writeIORef pos (( p + 1) `mod` cap)
        pure v

push :: RingBuffer a -> a -> IO ()
push (RB _ end size cap rb) v = do
  s <- readIORef size
  e <- readIORef end
  if s == cap
     then error "RingBuffer is full"
     else do
       MV.unsafeWrite rb e v
       writeIORef end ((e + 1) `mod` cap)
       writeIORef size (s + 1)

parseMachines :: Parser Word8 IO (Int, Int)
parseMachines = P.manyTill parseMachine P.eof sumBoth

parseMachine :: Parser Word8 IO (Int, Int)
parseMachine = do
  void $ P.satisfy (==_bracketleft)
  (i, rows) <- P.manyTill P.one (P.satisfy (==_bracketright)) toIndicator
  void $ P.satisfy (==_space)
  bs <- P.manyTill parseButton (P.satisfy (==_braceleft)) F.toList
  let b = map fst bs
      b' = map snd bs
  j <- parseJoltage rows b'
  void $ P.satisfy (==_lf)
  liftIO . solveBoth $ M i b j

parseButton :: Parser Word8 IO (Int, [Int])
parseButton = do
  void $ P.satisfy (==_parenleft)
  b <- P.deintercalate (P.satisfy isDigit) (P.satisfy (==_comma)) toButton
  void $ P.satisfy (==_parenright)
  void $ P.satisfy (==_space)
  pure b

parseJoltage :: Int -> [[Int]] -> Parser Word8 IO Matrix
parseJoltage rows buttons = do
  let lb = length buttons
      columns = lb + 1
  mx <- liftIO $ MV.replicateM rows (MV.replicate columns 0)
  let innerFold i _ b = do
        r <- MV.unsafeRead mx b
        MV.unsafeWrite r i 1
      buttonify i bs = do
        foldM_ (innerFold i) () bs
        pure (i + 1)
  liftIO $ foldM_ buttonify 0 buttons
  P.deintercalate (P.many (P.satisfy isDigit) toNumber) (P.satisfy (==_comma)) (eitherToList lb mx)
  void $ P.satisfy (==_braceright)
  pure $ Mx rows lb mx

toIndicator :: Fold IO Word8 (Int, Int)
toIndicator = flip F.foldr' (0, 0) $ \w (i, j) ->
  (i `shiftL` 1 + if w ==_numbersign then 1 else 0, j + 1)

toButton :: Fold IO (Either Word8 Word8) (Int, [Int])
toButton = flip F.foldl' (0, []) $ \b@(bi, bl) w ->
  case w of
    Left d  -> (bi `setBit` digit d, digit d : bl)
    Right _ -> b

toNumber :: Fold IO Word8 Int
toNumber = flip F.foldl' 0 $ \n w -> 10 * n + digit w

eitherToList :: Int -> IOVector (IOVector Int) -> Fold IO (Either Int Word8) ()
eitherToList col mx = void $ flip F.foldlM' (pure 0) $ \i v ->
  case v of
    Left n  -> do
      r <- MV.unsafeRead mx i
      MV.unsafeWrite r col n
      pure (i + 1)
    Right _ -> pure i

sumBoth :: Fold IO (Int, Int) (Int, Int)
sumBoth = F.foldl' (\(a, b) -> bimap (a +) (b +)) (0, 0)

solveBoth :: Machine  -> IO (Int, Int)
solveBoth (M i bs j@(Mx rows _ _)) = do
  rb <- newRingBuffer 1024 
  push rb (0, 0)
  seen <- MV.replicate (1 `shiftL` rows) False
  MV.unsafeWrite seen 0 True
  p1 <- bfsIndicators rb seen bs i 
  let p2 = 0
  hermiteNormalForm j
  traceMatrix j
  pure (p1, p2)

bfsIndicators :: RingBuffer (Int, Int) -> IOVector Bool -> [Int] -> Int -> IO Int
bfsIndicators toSee seen bs goal = bfsIndicators'
  where
    bfsIndicators' = do
      (c, cur) <- pop toSee
      if cur == goal
         then pure c
         else do 
           let folder _ b = do
                  let cur' = cur `xor` b
                  s <- MV.unsafeRead seen cur'
                  if s then pure ()
                       else do
                         push toSee (c + 1, cur')
                         MV.unsafeWrite seen cur' True
           foldM_ folder () bs
           bfsIndicators'

-- | Calculate the Hermite Normal Form of a Matrix using Cohen 2.4.4
-- note : unlike Cohen, we start from 0
hermiteNormalForm :: Matrix -> IO ()
hermiteNormalForm (Mx m n mx) = hnf (m - 1) (n - 1) $ max (n - m) 0
  where
    hnf k j l = do
      let finish v = when (j > l) $ hnf (v - 1) (j - 1) l -- Step 6
-- Step 2 :: if all the a_ij with i < k are0 then step 5 else step 3
      az <- foldrM (\v sc -> do
        r <- MV.unsafeRead mx v
        avj <- MV.unsafeRead r j
        pure (avj == 0 && sc)) 
        True [0..k - 1]
      if az
         then do
            rk <- MV.unsafeRead mx k
            akj <- MV.unsafeRead rk j
-- end of step 2, treat negative akj
            when (akj < 0) $ mapIOVector rk negate
-- step 5
            if akj == 0 
               then finish (k + 1)
               else do
                  reduce ( k + 1 ) ( m - 1 ) j n (abs akj) rk mx 
                  finish k
         else do
-- Step 3. choose the minimal aij for i <= k
            (mini, minv) <- foldrM (\i (mi, mv) -> do
              r <- MV.unsafeRead mx i
              aij <- MV.unsafeRead r j
              if aij /= 0 && abs aij < abs mv 
                 then pure (i, aij)
                 else pure (mi, mv)) (undefined, maxBound) [0..k]
-- Step 3b. If min_i < k then swap rows min_i and k. Then if our (maybe)
-- new akjis negative, negate the whole row.
            when (mini < k) $ MV.unsafeSwap mx mini k
            rk <- MV.unsafeRead mx k
            when (minv < 0) $ mapIOVector rk negate
            let akj = abs minv
-- Step 4. for i = 0 .. k -1, do a modular replacement
            reduce 0 (k - 1) j n akj rk mx
-- loop back to step 2
            hnf k j l

reduce :: Int -> Int -> Int -> Int -> Int -> IOVector Int -> IOVector (IOVector Int) -> IO ()
reduce f t j l akj rk mx = reduce' f
  where
    reduce' i
      | i > t = pure ()
      | otherwise = do
          ri <- MV.unsafeRead mx i
          aij <- MV.unsafeRead ri j
          let q = aij `div` akj
              reduce'' x
                | x > l = pure ()
                | otherwise = do
                    aix <- MV.unsafeRead ri x
                    akx <- MV.unsafeRead rk x
                    MV.unsafeWrite ri x (aix - q * akx)
                    reduce'' (x + 1)
          reduce'' 0
          reduce' (i + 1)

mapIOVector :: IOVector a -> (a -> a) -> IO ()
mapIOVector v f = mapIOVector' 0 (MV.length v)
  where
    mapIOVector' i n
      | i == n = pure ()
      | otherwise = MV.unsafeModify v f i >> mapIOVector' (i + 1) n

traceMatrix :: Matrix -> IO ()
traceMatrix (Mx rows columns mx) = do
  traceIO $ "Rows: " ++ show rows ++ " columns " ++ show columns
  fmx <- V.freeze mx
  ffmx <- mapM V.freeze fmx
  mapM_ (traceIO . show) ffmx

part1 :: Bool -> Stream IO Word8 -> IO ()
part1 _ s = S.parse parseMachines s >>= print . fst . either (\e -> error $ "Parser failed: " ++ show e) id

part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ s = S.parse parseMachines s >>= print . snd . either (\e -> error $ "Parser failed: " ++ show e) id
