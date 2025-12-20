module Day10
  ( part1
  , part2
  , parseMachines
  ) where

import           Control.Monad            (void, foldM_, when, zipWithM_, forM)
import           Data.Bifunctor           (bimap)
import           Data.Bits                (setBit, shiftL, xor, testBit)
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
import qualified Data.Vector.Mutable as MV (replicate, replicateM, unsafeWrite, unsafeRead, unsafeNew, unsafeSwap, unsafeModify, length, foldr, unsafeSlice, forM_, mapM_)
import qualified Data.Vector as V (freeze, toList, unsafeFreeze, (!), fromList, mapMaybeM, zipWith, imap)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Debug.Trace ( traceIO )
import Data.Foldable (foldrM)
import Data.Ratio ((%), Ratio)
import Data.List ((\\))
import Data.Vector (Vector)
import Helpers.Heap.Mutable (newHeap, Heap)
import qualified Helpers.Heap.Mutable as H (push, popMin)
import Data.Maybe (catMaybes)

data Machine = M {-# UNPACK #-} !Int ![Int] Matrix

data Matrix = Mx {-# UNPACK #-} !Int {-# UNPACK #-} !Int (IOVector (IOVector Int))

data RingBuffer a = RB !(IORef Int) !(IORef Int) !(IORef Int) {-# UNPACK #-} !Int !(IOVector a)

data PivotRow = PR {-# UNPACK #-} !(Ratio Int) !(Vector (Ratio Int)) deriving Show

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
  hermiteNormalForm j
  (free, m) <- matrixToPivotRows j
  p2 <- if free == 0 
    then pure . round $ getLinearSolution m (V.fromList [])
    else do
      allConstraints <- generateFreePivots free m
      let vs = V.fromList $ getVertices free allConstraints
      potVs <- V.mapMaybeM (gaussianElimination free allConstraints) vs
      let ls = getLinearSolution m
      heap <- newHeap (length potVs) (\(sol, _) (sol', _) -> compare sol sol')
      mapM_ (\v -> H.push heap (ls v, v)) potVs
      findBest heap allConstraints maxBound ls ((1 `shiftL` free) - 1)
  when (p2 == maxBound) $ traceMatrix j
  pure (p1, p2)

findBest :: Heap (Ratio Int, Vector (Ratio Int)) -> [PivotRow] -> Int -> (Vector (Ratio Int) -> Ratio Int) -> Int -> IO Int
findBest heap constraints curBest ls hcSize = do
  mm <- H.popMin heap
  case mm of
    Nothing -> pure curBest
    Just (curS, curV) -> do
      if floor curS >= curBest
        then pure curBest
        else do
          let pick :: Int -> Int -> Ratio Int -> Int
              pick mask i x = if testBit mask i then ceiling x else floor x
              hypercube = filter (\v -> (round v :: Int) == (floor v :: Int)) $ catMaybes [ls <$> checkConstraints (V.imap (\i  x -> fromIntegral $ pick mask i x) curV) constraints | mask <- [0.. hcSize]]
              minCube = if null hypercube 
                          then Nothing
                          else pure $ minimum hypercube
          case minCube of
            Nothing -> findBest heap constraints curBest ls hcSize
            Just mc -> do
              let rmc = round mc
              if rmc >= curBest 
                then findBest heap constraints curBest ls hcSize
                else findBest heap constraints rmc ls hcSize

getLinearSolution :: [PivotRow] -> Vector (Ratio Int) -> Ratio Int
getLinearSolution [] freeVars = sum freeVars
getLinearSolution ((PR goal coefs) : ps) freeVars = goal - sum (V.zipWith (*) coefs freeVars) + getLinearSolution ps freeVars

generateFreePivots :: Int -> [PivotRow]-> IO [PivotRow]
generateFreePivots free pivots = gfp 0
  where
    gfp p
     | p == free = pure pivots
     | otherwise = do
         pv <- MV.replicate free 0
         MV.unsafeWrite pv p (-1)
         fpv <- V.freeze pv
         (PR 0 fpv :) <$> gfp (p + 1)

getVertices :: Int -> [PivotRow] -> [[PivotRow]]
getVertices 0 _ = [[]]
getVertices _ [] = []
getVertices k (p:ps) = map (p:) (getVertices (k - 1) ps) ++ getVertices k ps

gaussianElimination :: Int -> [PivotRow] -> [PivotRow] -> IO (Maybe (Vector (Ratio Int)))
gaussianElimination free constraints pivots = do
  m <- MV.replicateM free (MV.unsafeNew free)
  b <- MV.unsafeNew free
  x <- MV.unsafeNew free
  let buildVectors n []
        | n == free = pure ()
        | otherwise = error $ "Wrong number of vectors. Found " ++ show n ++ " out of " ++ show free ++ " expected."
      buildVectors n ((PR goal coeffs):ps) = do
        MV.unsafeWrite b n goal
        mr <- MV.unsafeRead m n
        writeRow mr 0 coeffs
        buildVectors (n + 1) ps
      writeRow mr i coeffs
        | i == free = pure ()
        | otherwise = do
            let co = coeffs V.! i
            MV.unsafeWrite mr i co
            writeRow mr (i + 1) coeffs
      az mi j
        | j == free = pure Nothing
        | otherwise = do
            mij <- MV.unsafeRead mi j
            if mij == 0
              then az mi (j + 1)
              else pure (Just j)
      swap i j l
        | l == free = do
            bi <- MV.unsafeRead b i
            bj <- MV.unsafeRead b j
            MV.unsafeWrite b i bj
            MV.unsafeWrite b j bi
        | otherwise = do
            ml <- MV.unsafeRead m l
            mli <- MV.unsafeRead ml i
            mlj <- MV.unsafeRead ml j
            MV.unsafeWrite ml i mlj
            MV.unsafeWrite ml j mli
            swap i j (l + 1)
      fillC mi d i bi k
        | k == free = pure ()
        | otherwise = do
            mik <- MV.unsafeRead mi k
            let ck = d * mik
            spreadD ck i k (i + 1)
            bk <- MV.unsafeRead b k
            MV.unsafeWrite b k (bk - ck * bi)
            fillC mi d i bi (k + 1)
      solveX j
        | j < 0 = do
            fx <- V.freeze x
            pure $ checkConstraints fx constraints
        | otherwise = do
            bj <- MV.unsafeRead b j
            sumPrevs <- fmap sum . forM [j + 1 .. free - 1] $ \i -> do
              mi <- MV.unsafeRead m i
              mij <- MV.unsafeRead mi j
              xi <- MV.unsafeRead x i
              pure $ mij * xi
            mj <- MV.unsafeRead m j
            mjj <- MV.unsafeRead mj j
            MV.unsafeWrite x j ((bj - sumPrevs) / mjj)
            solveX (j - 1)
      spreadD ck i k l
        | l == free =  pure ()
        | otherwise = do
            ml <- MV.unsafeRead m l
            mlk <- MV.unsafeRead ml k
            mli <- MV.unsafeRead ml i
            MV.unsafeWrite ml k (mlk - ck * mli)
            spreadD ck i k (l + 1)
      cohen i
        |i == free = solveX (free - 1)
        | otherwise = do -- implementing Cohen 2.2.1. Probably overkill
            mi <- MV.unsafeRead m i
            mj <- az mi i
            case mj of
              Nothing -> pure Nothing
              Just j -> do
                when (j > i) $ swap i j i
                mii <- MV.unsafeRead mi i
                let d = 1 / mii
                bi <- MV.unsafeRead b i
                fillC mi d i bi (i + 1)
                cohen (i + 1)
  buildVectors 0 pivots
  cohen 0

checkConstraints :: Vector (Ratio Int) -> [PivotRow] -> Maybe (Vector (Ratio Int))
checkConstraints x [] = Just x
checkConstraints x ((PR goal coefs):cs)
  | cx <= goal = checkConstraints x cs
  | otherwise = Nothing
  where
    cx = sum . V.zipWith (*) x $ coefs

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
                if akj == 0
                  then step2a rk akj (j - 1)
                  else do
-- end of step 2, treat negative akj
                    when (akj < 0) $ mapIOVector rk negate
                    step5 rk akj
            else step3
          -- first we need to check if we have a non null pivot in the [0..k]/[0..j-1] quadrant, in which case we'll swap columns
          step2a rk akj v
            | v < 0 = step5 rk akj
            | otherwise = do
                akv <- MV.unsafeRead rk v
                if akv == 0
                  then step2a rk akj (v - 1)
                  else do
                    MV.forM_ mx (\r -> MV.unsafeSwap r j v)
                    step2
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
            reduce 0 (k - 1) j n akj rk mx
            hnf k j l
          step5 rk akj = do
            if akj == 0
              then step6 (k + 1)
              else do
                reduce ( k + 1 ) ( m - 1 ) j n (abs akj) rk mx
                step6 k
          step6 v = when (j > l) $ hnf (v - 1) (j - 1) l -- Step 6
      step2

matrixToPivotRows :: Matrix -> IO (Int, [PivotRow])
matrixToPivotRows m@(Mx rows columns mx) = do
  (rank, free) <- calcRankAndFree m
  pushFree free m
  let offset = rows - rank
      pivotLine [] _ _ acc = pure (free, reverse acc)
      pivotLine (r:rs) prevPivots prevRhs acc = do
        row <- MV.unsafeRead mx (r + offset)
        curRhs <- MV.unsafeRead row columns
        diag <- MV.unsafeRead row (free + r)
        freeCs <- map (% diag) . V.toList <$> V.unsafeFreeze (MV.unsafeSlice 0 free row)
        preCs <- map (% diag) . reverse . V.toList <$> V.unsafeFreeze (MV.unsafeSlice free r row)
        let propagCoeffs = foldl' (zipWith (+)) freeCs $ zipWith (\a c -> map (a *) c) preCs prevPivots
            rhs = curRhs % diag - sum (zipWith (*) prevRhs preCs)
        pivotLine rs (propagCoeffs:prevPivots) (rhs:prevRhs) (PR rhs (V.fromList propagCoeffs) : acc)
  pivotLine [0..rank - 1] [] [] []

calcRankAndFree :: Matrix -> IO (Int, Int)
calcRankAndFree (Mx rows columns mx) = do
  let countEmpty i = do
        ri <- MV.unsafeRead mx i
        az <- MV.foldr (\v t -> v == 0 && t) True ri
        if az
          then countEmpty (i + 1)
          else pure i
  emp <- countEmpty 0
  let rank = rows - emp
      free = columns - rank
  pure (rank, free)

pushFree :: Int -> Matrix -> IO ()
pushFree free (Mx rows columns mx)
  | free == 0 = pure ()
  | otherwise = do
      frees <- getFree (rows - 1) (columns - 1)
      when (length frees /= free) $ error $ " found " ++ show (length frees) ++ " free columns: " ++ show frees ++ " instead of " ++ show free
      let pivots = [0..columns - 1] \\ frees
      MV.mapM_ (pushCols (frees ++ pivots)) mx
    where
      getFree r c
        | c < 0 = pure []
        | r < 0 = pure [0..c]
        | otherwise = do
            rr <- MV.unsafeRead mx r
            arc <- MV.unsafeRead rr c
            if arc == 0
              then (c :) <$> getFree r (c - 1)
              else getFree (r - 1) (c - 1)
      pushCols newCols r = do
        old <- V.freeze r
        zipWithM_ (\i o -> MV.unsafeWrite r i (old V.! o)) [0..] newCols

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
