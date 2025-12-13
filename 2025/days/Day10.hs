module Day10
  ( part1
  , part2
  , parseMachines
  ) where

import           Control.Monad            (void, foldM_, (>=>))
import           Data.Bifunctor           (bimap)
import           Data.Bits                (setBit, shiftL, xor, (.&.), shiftR)
import qualified Data.IntSet              as IS (insert, member, singleton)
import           Data.Sequence            (Seq (..), ViewL (EmptyL, (:<)))
import qualified Data.Sequence            as Sq (singleton, viewl, length)
import           Data.Word                (Word8)
import           Data.Word8               (_braceleft, _braceright,
                                           _bracketleft, _bracketright, _comma,
                                           _lf, _numbersign, _parenleft,
                                           _parenright, _space)
import           Helpers.General.Streamly (digit, isDigit)
import qualified Streamly.Data.Fold       as F (foldl', foldr')
import           Streamly.Data.Fold       (Fold)
import qualified Streamly.Data.Parser     as P (deintercalate, eof, many,
                                                manyTill, one, satisfy)
import           Streamly.Data.Parser     (Parser)
import qualified Streamly.Data.Stream     as S (parse)
import           Streamly.Data.Stream     (Stream)
import Control.Monad.IO.Class (liftIO)
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as MV (unsafeRead, unsafeWrite, replicateM, length, replicate, mapM_, unsafeSwap, imapM_)
import qualified Data.Vector as V (fromList, thaw, freeze)
import Debug.Trace

data Machine = M {-# UNPACK #-} !Int !(Seq Int) ![Int]

data RBFS = Failure Int | Success Int Int | Prune deriving Show

parseMachines :: Parser Word8 IO (Int, Int)
parseMachines = P.manyTill parseMachine P.eof sumBoth

parseMachine :: Parser Word8 IO (Int, Int)
parseMachine = do
  void $ P.satisfy (==_bracketleft)
  i <- P.manyTill P.one (P.satisfy (==_bracketright)) toIndicator
  void $ P.satisfy (==_space)
  b <- P.manyTill parseButton (P.satisfy (==_braceleft)) toSeq
  j <- parseJoltage
  void $ P.satisfy (==_lf)
  liftIO $ bfsBoth $ M i b j

parseButton :: Parser Word8 IO Int
parseButton = do
  void $ P.satisfy (==_parenleft)
  b <- P.deintercalate (P.satisfy isDigit) (P.satisfy (==_comma)) toButton
  void $ P.satisfy (==_parenright)
  void $ P.satisfy (==_space)
  pure b

parseJoltage :: Parser Word8 IO [Int]
parseJoltage = do
  j <- P.deintercalate (P.many (P.satisfy isDigit) toNumber) (P.satisfy (==_comma)) eitherToList
  void $ P.satisfy (==_braceright)
  pure j

toIndicator :: Fold IO Word8 Int
toIndicator = flip F.foldr' 0 $ \w i -> (i `shiftL` 1) +
  if w ==_numbersign then 1 else 0

toSeq :: Fold IO Int (Seq Int)
toSeq = F.foldl' (:|>) mempty

toButton :: Fold IO (Either Word8 Word8) Int
toButton = flip F.foldl' 0 $ \b w ->
  case w of
    Left d  -> b `setBit` digit d
    Right _ -> b

toNumber :: Fold IO Word8 Int
toNumber = flip F.foldl' 0 $ \n w -> 10 * n + digit w

eitherToList :: Fold IO (Either Int Word8) [Int]
eitherToList = flip F.foldl' [] $ \ns v ->
  case v of
    Left n  -> n : ns
    Right _ -> ns

sumBoth :: Fold IO (Int, Int) (Int, Int)
sumBoth = F.foldl' (\(a, b) -> bimap (a +) (b +)) (0, 0)

bfsBoth :: Machine  -> IO (Int, Int)
bfsBoth (M i bs j) = do
  let p1 = bfsIndicator i bs
      p2 = 0 :: Int
  (mat, vec) <- matrixify bs j
  V.freeze vec >>= traceIO.show
  MV.mapM_ (V.freeze >=> (traceIO. show)) mat
  traceIO "\n"
  pure (p1, p2)

bfsIndicator :: Int -> Seq Int -> Int
bfsIndicator i bs = bfs (Sq.singleton (0, 0)) (IS.singleton 0)
  where
    bfs toSee seen = if s == i then c else bfs toSee' seen'
      where
        (s, c, rest) = case Sq.viewl toSee of
                            EmptyL        -> undefined
                            ((v, a) :< r) -> (v, a, r)
        (toSee', seen') = foldr buildNext (rest, seen) bs
        buildNext b (t, st)
          | b' `IS.member` seen = (t, st)
          | otherwise = (t :|> (b', c + 1), IS.insert b' st)
          where
            b' = s `xor` b

matrixify :: Seq Int -> [Int] -> IO (IOVector (IOVector Int), IOVector Int)
matrixify sq vals = do
  vec <- V.thaw . V.fromList . reverse $ vals
  let lv = MV.length vec
      ls = Sq.length sq
  mat <- MV.replicateM lv (MV.replicate ls 0)
  let foldInt _ _ 0 = pure ()
      foldInt i j v = MV.unsafeRead mat i
                        >>= \r -> MV.unsafeWrite r j (v .&. 1)
                        >> foldInt (i + 1) j (v `shiftR` 1)
  foldM_ (\j v -> foldInt 0 j v >> pure (j + 1)) 0 sq
  V.freeze vec >>= traceIO.show
  MV.mapM_ (V.freeze >=> (traceIO. show)) mat
  traceIO "\n"
  reduce mat vec
  pure (mat, vec)

reduce :: IOVector (IOVector Int) -> IOVector Int -> IO ()
reduce mat vec = do 
  lc <- MV.length <$> MV.unsafeRead mat 0
  let lr = MV.length mat
      crawl k r
        | r == lr = pure Nothing
        | otherwise = do
            cr <- MV.unsafeRead mat r
            kv <- MV.unsafeRead cr k
            if kv == 0 then crawl k (r + 1)
                       else pure $ Just r
      reduce' k n
        | k == min lr lc = pure ()
        | otherwise = do
            order k k 
            kr <- MV.unsafeRead mat k
            kv <- MV.unsafeRead kr k
            if kv == 0 && k >= lc - n
               then pure ()
               else if kv == 0
                then do
                  traceIO $ "k = " ++ show k ++ " and n " ++ show n
                  MV.mapM_ (\r -> MV.unsafeSwap r k (lc - n)) mat
                  reduce' k (n + 1)
                else do
                  pivot k kv kr 0 
                  traceIO $ " k = " ++ show k
                  V.freeze vec >>= traceIO.show
                  MV.mapM_ (V.freeze >=> (traceIO. show)) mat
                  traceIO "\n"
                  reduce' (k + 1) n
      order k r
        | r == lr = pure ()
        | otherwise = do
            curR <- MV.unsafeRead mat r
            kv <- MV.unsafeRead curR k
            if kv == 0
               then do
                 nv <- crawl k ( r + 1 )
                 case nv of
                   Nothing -> pure ()
                   Just sw -> do 
                     MV.unsafeSwap mat r sw 
                     MV.unsafeSwap vec r sw
                     order k (r + 1)
               else order k (r + 1)
      pivot k kv kr r
        | r == lr = pure ()
        | r == k = pivot k kv kr (r + 1)
        | otherwise = do
            rr <- MV.unsafeRead mat r
            rv <- MV.unsafeRead rr k
            if rv == 0 
               then pure ()
               else do
                 vkv <- MV.unsafeRead vec k
                 vrv <- MV.unsafeRead vec r
                 MV.unsafeWrite vec r (kv * vrv - rv * vkv) 
                 MV.imapM_ (pivot' kr rr kv rv) rr
                 pivot k kv kr (r + 1)
      pivot' kr rr kv rv i v = do
        kiv <- MV.unsafeRead kr i
        MV.unsafeWrite rr i (kv * v - rv * kiv)
  reduce' 0 1

part1 :: Bool -> Stream IO Word8 -> IO ()
part1 _ s = S.parse parseMachines s >>= print . fst . either (\e -> error $ "Parser failed: " ++ show e) id

part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ s = S.parse parseMachines s >>= print . snd . either (\e -> error $ "Parser failed: " ++ show e) id
