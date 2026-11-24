module Day10
  ( part1
  , part2
  , parseMachines
  ) where

import           Control.Monad            (foldM_, unless, void, when, (>=>))
import           Control.Monad.IO.Class   (liftIO)
import           Data.Bifunctor           (bimap)
import           Data.Bits                (setBit, shiftL, xor)
import           Data.Foldable            (foldrM)
import           Data.IORef               (IORef, newIORef, readIORef,
                                           writeIORef)
import           Data.Maybe               (mapMaybe)
import           Data.Vector              (Vector)
import qualified Data.Vector              as V (freeze, length, thaw, (!))
import           Data.Vector.Mutable      (IOVector)
import qualified Data.Vector.Mutable      as MV (drop, foldr, length, replicate,
                                                 replicateM, splitAt, take,
                                                 unsafeModify, unsafeNew,
                                                 unsafeRead, unsafeSwap,
                                                 unsafeWrite)
import           Data.Word                (Word8)
import           Data.Word8               (_braceleft, _braceright,
                                           _bracketleft, _bracketright, _comma,
                                           _lf, _numbersign, _parenleft,
                                           _parenright, _space)
import           Helpers.General.Streamly (digit, isDigit)
import qualified Streamly.Data.Fold       as F (foldl', foldlM', foldr', toList)
import           Streamly.Data.Fold       (Fold)
import qualified Streamly.Data.Parser     as P (deintercalate, eof, many,
                                                manyTill, one, satisfy)
import           Streamly.Data.Parser     (Parser)
import qualified Streamly.Data.Stream     as S (parse)
import           Streamly.Data.Stream     (Stream)

data Machine = M {-# UNPACK #-} !Int ![Int] Matrix

data Matrix = Mx {-# UNPACK #-} !Int {-# UNPACK #-} !Int (IOVector (IOVector Int)) (Vector Int)

data RingBuffer a = RB !(IORef Int) !(IORef Int) !(IORef Int) {-# UNPACK #-} !Int !(IOVector a)

type Ineq = ([Int], Int)

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
parseJoltage counters buttons = do
  let rows = length buttons
      columns = counters + rows
  mx <- liftIO $ MV.replicateM rows (MV.replicate columns 0)
  vc <- liftIO $ MV.replicate counters 0
  let innerFold r _ b = do
        MV.unsafeWrite r b 1
      buttonify i bs = do
        r <- MV.unsafeRead mx i
        foldM_ (innerFold r) () bs
        MV.unsafeWrite r (counters + i) 1
        pure (i + 1)
  liftIO $ foldM_ buttonify 0 buttons
  P.deintercalate (P.many (P.satisfy isDigit) toNumber) (P.satisfy (==_comma)) $ eitherToList vc
  void $ P.satisfy (==_braceright)
  vcf <- liftIO $ V.freeze vc
  pure $ Mx counters rows mx vcf

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

eitherToList :: IOVector Int -> Fold IO (Either Int Word8) ()
eitherToList vc = void $ flip F.foldlM' (pure 0) $ \k v ->
  case v of
    Left n  -> do
      MV.unsafeWrite vc k n
      pure (k + 1)
    Right _ -> pure k

sumBoth :: Fold IO (Int, Int) (Int, Int)
sumBoth = F.foldl' (\(a, b) -> bimap (a +) (b +)) (0, 0)

solveBoth :: Machine  -> IO (Int, Int)
solveBoth (M i bs j@(Mx rows _ _ _)) = do
  rb <- newRingBuffer 1024
  push rb (0, 0)
  seen <- MV.replicate (1 `shiftL` rows) False
  MV.unsafeWrite seen 0 True
  p1 <- bfsIndicators rb seen bs i
  hermiteNormalForm j
  (k, x0) <- findSingleSolution j
  let p2 = case findBest k x0 of
             Nothing -> error "No solution found"
             Just v  -> v
  pure (p1, p2)

findBest :: [Vector Int] -> Vector Int -> Maybe Int
findBest k x0 = recurseSys ineqs objective
  where
    objective = (map sum k, sum x0)
    ineq b is = (map (V.! b) k, x0 V.! b):is
    ineqs = foldr ineq [] [0..V.length x0 - 1]

recurseSys :: [Ineq] -> Ineq -> Maybe Int
recurseSys sys ([], r)
  | any ((< 0 ) . snd) sys = Nothing
  | otherwise = Just r
recurseSys sys ([a], c) = case getInterval sys of
                            Nothing -> Nothing
                            Just (lo, hi) -> Just (c + a * (if a > 0 then lo else hi))
recurseSys sys obj = case getInterval sys of
                       Nothing -> Nothing
                       Just (lo, hi) -> let potentials = mapMaybe try [lo .. hi] in
                                            if null potentials then Nothing else Just . minimum $ potentials
  where
    try v = recurseSys (map (substitute v) sys) (substitute v obj)

substitute :: Int -> Ineq -> Ineq
substitute v (a : is, c) = (is, c + a * v)
substitute _ _           = error "empty subtitution list"

eliminateVariable :: [Ineq] -> [Ineq]
eliminateVariable ineqs = removeNulls ++ combinePosNeg
  where
    split (a : e : xs, c) = (a, e, xs, c)
    split _ = error "not enough coefficients"
    splitIneqs = map split ineqs
    (pos, neg, nulls) = foldr posNegIneq ([], [], []) splitIneqs
    posNegIneq i@(_, e, _, _) (p, n, nl)
      | e > 0 = (i : p, n, nl)
      | e < 0 = (p, i : n, nl)
      | otherwise = (p, n, i : nl)
    removeNulls = map (\(a, _, xs, c) -> (a:xs, c)) nulls
    combinePosNeg = combine <$> neg <*> pos

combine :: (Int, Int, [Int], Int) -> (Int, Int, [Int], Int) -> Ineq
combine (na, n, nxs, nc) (pa, p, pxs, pc) = (a : xs, c)
  where
    q = negate n
    a = q * pa + p * na
    c = q * pc + p * nc
    xs = zipWith (\nn pp -> q * pp + p * nn) nxs pxs

getInterval :: [Ineq] -> Maybe (Int, Int)
getInterval [] = Nothing
getInterval ineqs@((i, _):_)
  | length i > 1 = getInterval . eliminateVariable $ ineqs
  | otherwise = case sorted of
                  Nothing -> Nothing
                  Just (lo, hi) -> bestLoHi lo hi
  where
    sorted = foldr sortIneq (Just ([], [])) ineqs
    bestLoHi lo hi
      | null hi || null lo || mh < ml = Nothing
      | otherwise = Just (ml, mh)
      where
        mh = minimum hi
        ml = maximum lo
    sortIneq _ Nothing = Nothing
    sortIneq (a:_, c) (Just (l, h))
      | a > 0 = Just (negate (c `div` a) : l, h)
      | a < 0 = Just (l, c `div` negate a : h)
      | a == 0 && c >= 0 = Just (l, h)
      | otherwise = Nothing
    sortIneq ([], _) _ = error "Where did you get that empty list from"

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
hermiteNormalForm (Mx counters buttons mx _) = hnf (buttons - 1) (counters - 1) 0
  where
    hnf k j l = do
      let step2 = do
            az <- foldrM (\v sc -> do
                    r <- MV.unsafeRead mx v
                    avj <- MV.unsafeRead r j
                    pure (avj == 0 && sc))
                  True [0..k - 1]
            if az
            then do
                rk <- MV.unsafeRead mx k
                akj <- MV.unsafeRead rk j
                when (akj < 0) $ mapIOVector rk negate
                step5 rk akj
            else step3
          -- check whether we have a valid pivot to the left of our current column.
          step3 = do
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
            step4 rk akj
          step4 rk akj = do
            reduce 0 (k - 1) j (counters + buttons - 1) akj rk mx
            hnf k j l
          step5 rk akj = do
            if akj == 0
              then step6 (k + 1)
              else do
                reduce ( k + 1 ) ( buttons - 1 ) j (counters + buttons - 1) (abs akj) rk mx
                step6 k
          step6 v = when (j > l && v > 0) $ hnf (v - 1) (j - 1) l -- Step 6
      step2

findSingleSolution :: Matrix -> IO ([Vector Int], Vector Int)
findSingleSolution (Mx counters buttons mx vc) = do
  let countEmpty i = do
        ri <- MV.take counters <$> MV.unsafeRead mx i
        az <- MV.foldr (\v t -> v == 0 && t) True ri
        if az
          then countEmpty (i + 1)
          else pure i
  emp <- countEmpty 0
  let pivs = MV.drop emp mx
  k <- mapM (MV.unsafeRead mx >=> V.freeze . MV.drop counters) [0 .. emp - 1]
  x0 <- MV.replicate buttons 0
  rest <- V.thaw vc
  let fill i = if i < 0
                  then pure ()
               else do
                      (ip, ci) <- MV.splitAt counters <$> MV.unsafeRead pivs i
                      (p, pivot) <- findPivot ip (counters - 1)
                      rp <- MV.unsafeRead rest p
                      when (rp `mod` pivot /= 0) $ error "no integral solution"
                      let coef = rp `div` pivot
                      treatRest ip coef 0
                      treatSol ci coef 0
                      fill (i - 1)
      findPivot row col =
        if col < 0
           then error "didn't find a pivot"
           else do
             val <- MV.unsafeRead row col
             case val of
               0 -> findPivot row (col - 1)
               p -> pure (col, p)
      treatRest ip coef c
        | c == counters = pure ()
        | otherwise = do
            rc <- MV.unsafeRead rest c
            ipc <- MV.unsafeRead ip c
            MV.unsafeWrite rest c (rc - coef * ipc)
            treatRest ip coef (c + 1)
      treatSol ci coef b
        | b == buttons = pure ()
        | otherwise = do
            xb <- MV.unsafeRead x0 b
            cb <- MV.unsafeRead ci b
            MV.unsafeWrite x0 b (xb + coef * cb)
            treatSol ci coef (b + 1)
  fill (MV.length pivs - 1)
  allNull <- MV.foldr (\a b -> (a == 0) && b) True rest
  unless allNull $ error "remainder is not empty"
  fx0 <- V.freeze x0
  pure (k, fx0)

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

part1 :: Bool -> Stream IO Word8 -> IO ()
part1 _ s = S.parse parseMachines s >>= print . fst . either (\e -> error $ "Parser failed: " ++ show e) id

part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ s = S.parse parseMachines s >>= print . snd . either (\e -> error $ "Parser failed: " ++ show e) id
